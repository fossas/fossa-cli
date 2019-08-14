package nodejs

import (
	"os"
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/buildtools/yarn"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

var NodeAnalyzer = module.AnalyzerV2{
	DiscoverFunc: NewDiscover,
	Strategies: module.Strategies{
		Named: map[module.StrategyName]module.Strategy{
			"yarn":               AnalyzeYarnCmd,
			"npm":                AnalyzeNpmCmd,
			"yarn.lock":          AnalyzeYarnLock,
			"package-lock.json":  AnalyzeNpmLock,
			"node_modules":       AnalyzeNodeModules,
			"node_modules_local": AnalyzeNodeModulesLocal,
			"package.json":       AnalyzePackageJson,
		},
		Optimal: []module.StrategyName{"yarn", "npm", "yarn.lock", "package-lock.json"},
		SortedNames: []module.StrategyName{
			"yarn",
			"yarn.lock",
			"npm",
			"package-lock.json",
			"node_modules",
			"node_modules_local",
			"package.json",
		},
	},
}

func NewDiscover(dir module.Filepath) (map[module.Filepath][]module.DiscoveredStrategy, *errors.Error) {
	log.WithField("dir", dir).Debug("discovering modules")
	modules := make(map[module.Filepath][]module.DiscoveredStrategy)
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		addStrategies := func (strategies ...module.DiscoveredStrategy) {
			moduleDir := filepath.Dir(path)
			current := modules[moduleDir]
			current = append(current, strategies...)
			modules[moduleDir] = current
		}

		if info.IsDir() && info.Name() == "node_modules" {
			addStrategies(
				module.DiscoveredStrategy{
					Name:      "node_modules",
					RelTarget: "node_modules",
				},
				module.DiscoveredStrategy{
					Name:      "node_modules_local",
					RelTarget: "node_modules",
				},
			)
		}

		// Don't descend into **/node_modules and **/bower_components
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "package.json" {
			// TODO: add `npm` strategy as well?
			addStrategies(module.DiscoveredStrategy{
				Name:      "package.json",
				RelTarget: "package.json",
			})
		}

		if !info.IsDir() && info.Name() == "yarn.lock" {
			addStrategies(
				module.DiscoveredStrategy{
					Name:      "yarn.lock",
					RelTarget: "yarn.lock",
				},
				module.DiscoveredStrategy{
					Name:      "yarn",
					RelTarget: "yarn.lock",
				},
			)
		}

		if !info.IsDir() && info.Name() == "package-lock.json" {
			addStrategies(
				module.DiscoveredStrategy{
					Name:      "package-lock.json",
					RelTarget: "package-lock.json",
				},
				module.DiscoveredStrategy{
					Name:      "npm",
					RelTarget: "package-lock.json",
				},
			)
		}

		return nil
	})

	if err != nil {
		return nil, errors.UnknownError(err, "could not find NodeJS projects")
	}

	return modules, nil
}

// path is path to package.json
func AnalyzeYarnCmd(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	// TODO
	return graph.Deps{}, errors.NotImplementedError()
}

func AnalyzeNpmCmd(dir module.Filepath, _ module.Filepath) (graph.Deps, *errors.Error) {
	// TODO: mockability?
	npmcli, err := npm.New()
	if err != nil {
		// TODO: better error
		return graph.Deps{}, errors.UnknownError(err, "Couldn't find NPM")
	}

	if !npmcli.Exists() {
		// TODO: better error
		return graph.Deps{}, errors.UnknownError(err, "Couldn't find NPM")
	}

	pkgs, err := npmcli.List(dir)
	if err != nil {
		// TODO: better error
		return graph.Deps{}, errors.UnknownError(err, "Couldn't scan with npm cli")
	}

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

// target is yarn.lock
func AnalyzeYarnLock(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	deps, err := yarn.FromProject(filepath.Join(dir, "package.json"), filepath.Join(target))
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "Couldn't scan yarn.lock")
	}

	return deps, nil
}

func AnalyzeNpmLock(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	// TODO
	return graph.Deps{}, errors.NotImplementedError()
}

// target is node_modules
func AnalyzeNodeModules(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	// TODO: this seems to introduce an infinite loop
	return graph.Deps{}, errors.NotImplementedError()
	/*dir := filepath.Dir(path) // TODO: pass in the module path as well?
	deps, err := npm.FromNodeModules(dir, "package.json")
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "Couldn't scan node_modules")
	}

	return deps, nil*/
}

// target is node_modules
func AnalyzeNodeModulesLocal(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	// TODO
	return graph.Deps{}, errors.NotImplementedError()
}

// target is package.json
func AnalyzePackageJson(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	// TODO
	return graph.Deps{}, errors.NotImplementedError()
}

