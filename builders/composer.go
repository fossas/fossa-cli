package builders

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

// ComposerBuilder implements Builder for Composer (composer.json) builds
type ComposerBuilder struct {
	PHPCmd     string
	PHPVersion string

	ComposerCmd     string
	ComposerVersion string
}

// Initialize collects metadata on PHP and Composer binaries
func (builder *ComposerBuilder) Initialize() error {
	log.Logger.Debug("Initializing Composer builder...")

	// Set PHP context variables
	phpCmd, phpVersion, err := which("-v", os.Getenv("PHP_BINARY"), "php")
	if err != nil {
		log.Logger.Warningf("Could not find PHP binary (try setting $PHP_BINARY): %s", err.Error())
	}
	builder.PHPCmd = phpCmd
	builder.PHPVersion = phpVersion

	// Set Composer context variables
	composerCmd, composerVersion, err := which("-V", os.Getenv("COMPOSER_BINARY"), "composer")
	if err != nil {
		return fmt.Errorf("could not find Composer binary (try setting $COMPOSER_BINARY): %s", err.Error())
	}
	builder.ComposerCmd = composerCmd
	builder.ComposerVersion = composerVersion

	log.Logger.Debugf("Done initializing Composer builder: %#v", builder)
	return nil
}

// Build runs `composer install --prefer-dist --no-dev` and cleans with `rm -rf vendor`
func (builder *ComposerBuilder) Build(m module.Module, force bool) error {
	log.Logger.Debug("Running Composer build: %#v %#v", m, force)

	if force {
		_, _, err := runLogged(m.Dir, "rm", "-rf", "vendor")
		if err != nil {
			return fmt.Errorf("could not remove Composer cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(m.Dir, builder.ComposerCmd, "install", "--prefer-dist", "--no-dev")
	if err != nil {
		return fmt.Errorf("could not run Composer build: %s", err.Error())
	}

	log.Logger.Debug("Done running Composer build.")
	return nil
}

type composerManifest struct {
	Name    string
	Version string
}

// Analyze parses the output of `composer show -f json --no-ansi`
func (builder *ComposerBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debug("Running Composer analysis: %#v %#v", m, allowUnresolved)

	// Run `composer show --format=json --no-ansi` to get resolved versions
	showOutput, _, err := runLogged(m.Dir, builder.ComposerCmd, "show", "--format=json", "--no-ansi")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from Composer: %s", err.Error())
	}
	composerJSON := map[string][]composerManifest{}
	err = json.Unmarshal([]byte(showOutput), &composerJSON)
	if err != nil {
		return nil, fmt.Errorf("could not parse dependency list as JSON: %#v %#v", err.Error(), showOutput)
	}
	revisionMap := make(map[string]string)
	for _, dep := range composerJSON["installed"] {
		revisionMap[dep.Name] = dep.Version
	}

	// Run `composer show --tree --no-ansi` to get paths
	treeOutput, _, err := runLogged(m.Dir, builder.ComposerCmd, "show", "--tree", "--no-ansi")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from Composer: %s", err.Error())
	}
	var depList []Imported
	root := module.Locator{
		Fetcher:  "root",
		Project:  "root",
		Revision: "root",
	}
	depContext := module.ImportPath{root}
	lastDepth := 0
	lines := strings.Split(treeOutput, "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		var locator module.Locator
		var depth int
		if line[0] != '`' && line[0] != '|' && line[0] != ' ' {
			// We're at a top-level package.
			parts := strings.Split(line, " ")
			locator = module.Locator{
				Fetcher:  "comp",
				Project:  parts[0],
				Revision: "revision-placeholder",
			}
			depContext = module.ImportPath{root, locator}
			depth = 0
		} else {
			// We're somewhere in the tree.
			r := regexp.MustCompile("^([ \\|`-]+)([^ \\|`-][^ ]+) (.*)$")
			matches := r.FindStringSubmatch(line)
			locator = module.Locator{
				Fetcher:  "comp",
				Project:  matches[2],
				Revision: "revision-placeholder",
			}
			if matches[2] == "php" {
				continue
			}
			depth = len(matches[1])
			if depth%3 != 0 {
				// Sanity check
				log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line, matches)
			}
		}
		if depth > lastDepth {
			depContext = append(depContext, locator)
		} else {
			depContext = depContext[:depth/3+1]
		}
		depList = append(depList, Imported{
			Locator: locator,
			From:    append(module.ImportPath{}, depContext...),
		})
		lastDepth = depth
	}
	deps := computeImportPaths(depList)

	// Resolve revisions
	for i, dep := range deps {
		deps[i].Revision = revisionMap[dep.Project]
		for j, path := range dep.Via {
			for k, component := range path {
				deps[i].Via[j][k].Revision = revisionMap[component.Project]
			}
		}
	}

	log.Logger.Debugf("Done running Composer analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether `composer show --no-ansi` produces output
func (builder *ComposerBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Logger.Debugf("Checking Composer build: %#v %#v", m, allowUnresolved)

	// Run `composer show --no-ansi`
	output, _, err := runLogged(m.Dir, builder.ComposerCmd, "show", "--no-ansi")
	if err != nil {
		return false, fmt.Errorf("could not get dependency list from Composer: %s", err.Error())
	}

	// Check that the output is non-empty
	isBuilt := len(strings.TrimSpace(string(output))) > 0

	log.Logger.Debugf("Done checking Composer build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *ComposerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for ComposerBuilder")
}

// DiscoverModules finds composer.json modules not a /vendor/ folder
func (builder *ComposerBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var moduleConfigs []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("failed to access path %s: %s\n", path, err.Error())
			return err
		}

		// Skip the /vendor/ folder
		if info.IsDir() && info.Name() == "vendor" {
			log.Logger.Debugf("skipping `vendor` directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "composer.json" {
			moduleName := filepath.Base(filepath.Dir(path))

			// Parse from composer.json and set moduleName if successful
			var composerPackage composerManifest
			if err := parseLogged(path, &composerPackage); err == nil {
				moduleName = composerPackage.Name
			}

			log.Logger.Debugf("found Composer package: %s (%s)", path, moduleName)
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: moduleName,
				Path: path,
				Type: string(module.Composer),
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find Composer package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}
