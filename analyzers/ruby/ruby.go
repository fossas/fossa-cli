package ruby

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/builders/builderutil"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

// RubyBuilder implements Builder for Bundler (Gemfile) builds
type RubyBuilder struct {
	BundlerCmd     string
	BundlerVersion string

	GemCmd     string
	GemVersion string

	RubyCmd     string
	RubyVersion string
}

// Initialize collects metadata on Ruby, Gem, and Bundler binaries
func (builder *RubyBuilder) Initialize() error {
	log.Logger.Debug("Initializing Ruby builder...")

	// Set Ruby context variables
	rubyCmd, rubyVersion, err := exec.Which("-v", os.Getenv("RUBY_BINARY"), "ruby")
	if err != nil {
		log.Logger.Warningf("Could not find Ruby binary (try setting $RUBY_BINARY): %s", err.Error())
	}
	builder.RubyCmd = rubyCmd
	builder.RubyVersion = rubyVersion

	// Set Gem context variables
	gemCmd, gemVersion, err := exec.Which("-v", os.Getenv("GEM_BINARY"), "gem")
	if err != nil {
		log.Logger.Warningf("Could not find Gem binary (try setting $GEM_BINARY): %s", err.Error())
	}
	builder.GemCmd = gemCmd
	builder.GemVersion = gemVersion

	// Set Bundler context variables
	bundlerCmd, bundlerVersion, err := exec.Which("-v", os.Getenv("BUNDLER_BINARY"), "bundler", "bundle")
	if err != nil {
		return fmt.Errorf("could not find Bundler binary (try setting $BUNDLER_BINARY): %s", err.Error())
	}
	builder.BundlerCmd = bundlerCmd
	builder.BundlerVersion = bundlerVersion

	log.Logger.Debugf("Initialized Ruby builder: %#v", builder)
	return nil
}

// Build runs `bundler install --deployment --frozen` and cleans with `rm Gemfile.lock`
func (builder *RubyBuilder) Build(m module.Module, force bool) error {
	log.Logger.Debugf("Running Ruby build: %#v %#v", m, force)

	if force {
		_, _, err := exec.Run(exec.Cmd{
			Dir:  m.Dir,
			Name: "rm",
			Argv: []string{"Gemfile.lock"},
		})
		if err != nil {
			return fmt.Errorf("could not remove Ruby cache: %s", err.Error())
		}
	}

	_, _, err := exec.Run(exec.Cmd{
		Dir:  m.Dir,
		Name: builder.BundlerCmd,
		Argv: []string{"install", "--deployment", "--frozen"},
	})
	if err != nil {
		return fmt.Errorf("could not run Ruby build: %s", err.Error())
	}

	log.Logger.Debug("Done running Ruby build.")
	return nil
}

type rubyGem struct {
	module.Locator
	imports map[string]rubyGem
}

func hydrateRubyGems(root rubyGem, edges map[string]map[string]bool, versions map[string]string) rubyGem {
	// Resolve version
	if root.Revision == "" {
		root.Revision = versions[root.Project]
	}

	// Hydrate edges
	hydrated := make(map[string]rubyGem)
	for edge, dep := range root.imports {
		hydrated[edge] = hydrateRubyGems(dep, edges, versions)
	}

	for edge := range edges[root.Project] {
		if _, ok := hydrated[edge]; ok {
			continue
		}
		hydrated[edge] = hydrateRubyGems(rubyGem{
			Locator: module.Locator{
				Fetcher:  "gem",
				Project:  edge,
				Revision: versions[edge],
			},
			imports: make(map[string]rubyGem),
		}, edges, versions)
	}
	root.imports = hydrated

	return root
}

func flattenRubyGems(root rubyGem, from module.ImportPath) []builderutil.Imported {
	var imports []builderutil.Imported
	for _, dep := range root.imports {
		imports = append(imports, flattenRubyGems(dep, append(from, root.Locator))...)
	}
	imports = append(imports, builderutil.Imported{
		Locator: root.Locator,
		From:    append(module.ImportPath{}, from...),
	})
	return imports
}

// Analyze parses a `Gemfile.lock`
func (builder *RubyBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Ruby analysis: %#v %#v", m, allowUnresolved)

	lockfileBytes, err := ioutil.ReadFile(path.Join(m.Dir, "Gemfile.lock"))
	if err != nil {
		return nil, errors.Wrap(err, "could not read Gemfile.lock")
	}

	edges := make(map[string]map[string]bool)
	versions := make(map[string]string)
	gitDeps := make(map[string]string)
	var directDeps []string

	sections := strings.Split(string(lockfileBytes), "\n\n")
	for _, section := range sections {
		lines := strings.Split(strings.TrimSpace(section), "\n")
		depRegex := regexp.MustCompile("^( *?)(\\S+?)( \\((.*?)\\))?$")
		header := lines[0]
		if header == "GIT" {
			// Git dependency
			remoteRegex := regexp.MustCompile("^ *remote: (.*?)$")
			project := remoteRegex.FindStringSubmatch(lines[1])[1]
			revisionRegex := regexp.MustCompile("^ *revision: (.*?)$")
			revision := revisionRegex.FindStringSubmatch(lines[2])[1]
			gitDeps[project] = revision
		} else if header == "PATH" {
			// Vendored dependency: not currently supported
			continue
		} else if header == "GEM" {
			// Gem dependencies
			var parent string
			for _, line := range lines[3:] {
				matches := depRegex.FindStringSubmatch(line)
				if len(matches[1]) == 4 {
					parent = matches[2]
					versions[matches[2]] = matches[4]
				} else if len(matches[1]) == 6 {
					if _, ok := edges[parent]; !ok {
						edges[parent] = make(map[string]bool)
					}
					edges[parent][matches[2]] = true
				} else {
					log.Logger.Panicf("bad depth: %#v %#v\n", line, matches)
				}
			}
		} else if header == "DEPENDENCIES" {
			// List of direct dependencies
			for _, line := range lines[1:] {
				if strings.HasSuffix(line, "!") {
					// Ignore "!" (i.e. non-gem) dependencies: we don't support vendored ones and git ones are already added
					continue
				}
				dep := depRegex.FindStringSubmatch(line)[2]
				directDeps = append(directDeps, dep)
			}
		} else {
			continue
		}
	}

	directImports := make(map[string]rubyGem)
	for _, dep := range directDeps {
		directImports[dep] = rubyGem{
			Locator: module.Locator{
				Fetcher:  "gem",
				Project:  dep,
				Revision: versions[dep],
			},
			imports: make(map[string]rubyGem),
		}
	}

	root := module.Locator{
		Fetcher: "root",
		Project: "root",
	}
	graph := hydrateRubyGems(rubyGem{
		Locator: root,
		imports: directImports,
	}, edges, versions)

	imports := flattenRubyGems(graph, module.ImportPath{})
	// Add to direct dependencies -- this assumes all git dependencies are top-level (exec.Which I think is generally correct)
	for project, revision := range gitDeps {
		imports = append(imports, builderutil.Imported{
			Locator: module.Locator{
				Fetcher:  "git",
				Project:  project,
				Revision: revision,
			},
			From: module.ImportPath{root},
		})
	}
	// Remove "root" module at the end of `imports`
	deps := builderutil.ComputeImportPaths(imports[:len(imports)-1])

	log.Logger.Debugf("Done running Ruby analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether `Gemfile.lock` exists
func (builder *RubyBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Logger.Debugf("Checking Ruby build: %#v %#v", m, allowUnresolved)

	ok, err := files.Exists(m.Dir, "Gemfile.lock")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Ruby build: %#v", ok)
	return ok, nil
}

// IsModule is not implemented
func (builder *RubyBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for RubyBuilder")
}

// DiscoverModules returns ModuleConfigs that match Gemfiles in the directory
func (builder *RubyBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	gemFilePaths, err := doublestar.Glob(filepath.Join(dir, "**", "Gemfile"))
	if err != nil {
		return nil, err
	}
	moduleConfigs := make([]module.Config, len(gemFilePaths))
	for i, path := range gemFilePaths {
		gemName := filepath.Base(filepath.Dir(path))
		// infer title from *.gemspec in directory if exists
		gemSpecs, err := doublestar.Glob(filepath.Join(filepath.Dir(path), "*.gemspec"))
		if err == nil && len(gemSpecs) > 0 {
			matchGemName := regexp.MustCompile(`\s+s\.name\s+=\s?"(\w+)"`)
			gemSpecContents, err := ioutil.ReadFile(gemSpecs[0])
			if err == nil {
				matches := matchGemName.FindStringSubmatch(string(gemSpecContents))
				// matches: [0] = full match, [1] capture group
				if len(matches) == 2 {
					log.Logger.Debugf("%v", matches[1])
					gemName = matches[1]
				}
			}
		}
		path, _ = filepath.Rel(dir, path)
		moduleConfigs[i] = module.Config{
			Name: gemName,
			Path: path,
			Type: "ruby",
		}
	}
	return moduleConfigs, nil
}
