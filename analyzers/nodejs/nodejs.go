// Package nodejs provides analyzers for Node.js projects.
//
// A Node.js project is defined as any folder with a `package.json`. A project
// may or may not have dependencies.
//
// A `BuildTarget` for Node.js is defined as the relative path to the directory
// containing the `package.json`, and the `Dir` is defined as the CWD for
// running build tools (like `npm` or `yarn`).
//
// `npm` and `yarn` are explicitly supported as first-class tools. Where
// possible, these tools are queried before falling back to other strategies.
//
// All Node.js projects are implicitly supported via `node_modules` parsing.
package nodejs

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/buildtools/yarn"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	NodeVersion string

	NPM  npm.NPM
	Yarn yarn.YarnTool

	Module  module.Module
	Options Options
}

// Options contains options for the `Analyzer`.
//
// The analyzer can use many different strategies. These are:
//
//   - `yarn`: Run and parse `yarn ls --json`.
//   - `npm`: Run and parse `npm ls --json`.
//   - `yarn.lock`: Parse `./yarn.lock`.
//   - `package-lock.json`: Parse `./package-lock.json`.
//   - `node_modules`: Parse `./package.json` and recursively look up
// 		 dependencies with `node_modules` resolution.
//   - `node_modules_local`: Parse manifests in `./node_modules``.
//   - `package.json`: Parse `./package.json`.
//
// If no strategies are specified, the analyzer will try each of these
// strategies in descending order.
type Options struct {
	Strategy    string `mapstructure:"strategy"`
	AllowNPMErr bool   `mapstructure:"allow-npm-err"`
}

// New configures Node, NPM, and Yarn commands.
func New(m module.Module) (*Analyzer, error) {
	log.WithField("options", m.Options).Debug("constructing analyzer")

	_, nodeVersion, nodeErr := exec.Which("-v", os.Getenv("FOSSA_NODE_CMD"), "node", "nodejs")
	if nodeErr != nil {
		log.Warnf("Could not find Node.JS: %s", nodeErr.Error())
	}

	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	npmTool, err := npm.New()
	if err != nil {
		log.Warn("Could not initialize npm tooling")
	}

	yarnTool, err := yarn.New()
	if err != nil {
		log.Warn("Could not initialze yarn tooling")
	}

	analyzer := Analyzer{
		NodeVersion: nodeVersion,

		NPM:  npmTool,
		Yarn: yarnTool,

		Module:  m,
		Options: options,
	}

	log.Debugf("Initialized Node.js analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover searches for `package.json`s not within a `node_modules` or
// `bower_components`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	log.WithField("dir", dir).Debug("discovering modules")
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		// Skip **/node_modules and **/bower_components
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "package.json" {
			name := filepath.Base(filepath.Dir(path))
			// Parse from project name from `package.json` if possible
			if manifest, err := npm.FromManifest(path, "package.json"); err == nil && manifest.Name != "" {
				name = manifest.Name
			}

			log.Debugf("Found NodeJS project: %s (%s)", path, name)
			path, err = filepath.Rel(dir, path)
			if err != nil {
				panic(err)
			}
			modules = append(modules, module.Module{
				Name:        name,
				Type:        pkg.NodeJS,
				BuildTarget: filepath.Dir(path),
				Dir:         filepath.Dir(path),
			})
		}
		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find NodeJS projects")
	}

	return modules, nil
}

// Clean removes `node_modules`.
func (a *Analyzer) Clean() error {
	return files.Rm(a.Module.Dir, "node_modules")
}

// Build runs `yarn install --production --frozen-lockfile` if there exists a
// `yarn.lock` and `yarn` is available. Otherwise, it runs
// `npm install --production`.
func (a *Analyzer) Build() error {
	log.Debugf("Running Node.js build: %#v", a.Module)

	// Prefer Yarn where possible.
	if ok, err := files.Exists(a.Module.Dir, "yarn.lock"); err == nil && ok && a.Yarn.Exists() {
		err := a.Yarn.Install(a.Module.Dir)
		if err != nil {
			return errors.Wrap(err, "could not run `yarn` build")
		}
	} else {
		if !a.NPM.Exists() {
			return errors.New("attempting to build using npm without npm tooling")
		}

		err := a.NPM.Install(a.Module.Dir)
		if err != nil {
			return errors.Wrap(err, "could not run `npm` build")
		}
	}

	log.Debug("Done running Node.js build.")
	return nil
}

// IsBuilt returns true if a project has a manifest and either has no
// dependencies or has a `node_modules` folder.
//
// Note that there could be very strange builds where this will produce false
// negatives (e.g. `node_modules` exists in a parent folder). There can also
// exist builds where this will produce false positives (e.g. `node_modules`
// folder does not include the correct dependencies). We also don't take
// $NODE_PATH into account during resolution.
//
// TODO: with significantly more effort, we can eliminate both of these
// situations.
func (a *Analyzer) IsBuilt() (bool, error) {
	log.Debugf("Checking Node.js build: %#v", a.Module)

	manifest, err := npm.FromManifest(a.Module.BuildTarget, "package.json")
	if err != nil {
		return false, errors.Wrap(err, "could not parse package manifest to check build")
	}

	if len(manifest.Dependencies) == 0 {
		log.Debugf("Done checking Node.js build: project has no dependencies")
		return true, nil
	}

	hasNodeModules, err := files.ExistsFolder(a.Module.Dir, "node_modules")
	if err != nil {
		return false, err
	}

	log.Debugf("Done checking Node.js build: %#v", hasNodeModules)
	return hasNodeModules, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Debugf("Running Nodejs analysis: %#v", a.Module)

	// if npm as a tool does not exist, skip this
	if a.NPM.Exists() {
		pkgs, err := a.NPM.List(filepath.Dir(a.Module.BuildTarget))
		if err == nil {
			// TODO: we should move this functionality in to the buildtool, and have it
			// return `pkg.Package`s.
			// Set direct dependencies.
			var imports []pkg.Import
			for name, dep := range pkgs.Dependencies {
				imports = append(imports, pkg.Import{
					Target: dep.From,
					Resolved: pkg.ID{
						Type:     pkg.NodeJS,
						Name:     name,
						Revision: dep.Version,
						Location: dep.Resolved,
					},
				})
			}

			// Set transitive dependencies.
			deps := make(map[pkg.ID]pkg.Package)
			recurseDeps(deps, pkgs)

			log.Debugf("Done running Nodejs analysis: %#v", deps)

			return graph.Deps{
				Direct:     imports,
				Transitive: deps,
			}, nil
		}

		log.Warnf("NPM had non-zero exit code: %s", err.Error())
		log.Debug("Using fallback of node_modules")
	}

	deps, err := npm.FromNodeModules(a.Module.BuildTarget, "package.json")
	if err == nil {
		return deps, nil
	}

	log.Warnf("Could not determine deps from node_modules")
	log.Debug("Using fallback of lockfile check")

	// currently only support yarn.lock
	return yarn.FromProject(filepath.Join(a.Module.BuildTarget, "package.json"), filepath.Join(a.Module.BuildTarget, "yarn.lock"))
}

// TODO: implement this generically in package graph (Bower also has an
// implementation)
func recurseDeps(pkgMap map[pkg.ID]pkg.Package, p npm.Output) {
	for name, dep := range p.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     name,
			Revision: dep.Version,
			Location: dep.Resolved,
		}
		// Handle previously seen (usually deduplicated) entries: see #257.
		previous := pkgMap[id]

		// Set direct imports.
		var imports []pkg.Import
		for name, i := range p.Dependencies {
			imports = append(imports, pkg.Import{
				Target: i.From,
				Resolved: pkg.ID{
					Type:     pkg.NodeJS,
					Name:     name,
					Revision: i.Version,
					Location: i.Resolved,
				},
			})
		}
		// Update map.
		// NOTE: We're assuming that each deduplicated dependency's imports will
		// only be listed once. This assumption might not be true. If it's not, then
		// we need to do a set union instead of a list concatenation.
		pkgMap[id] = pkg.Package{
			ID:      id,
			Imports: append(imports, previous.Imports...),
		}
		// Recurse in imports.
		recurseDeps(pkgMap, dep)
	}
}
