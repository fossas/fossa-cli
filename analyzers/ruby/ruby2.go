package ruby

import (
	"os"
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/bundler"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
)

const (
	AnalyzerName = "ruby"

	BundlerListStrategy     = "list"
	BundlerLockStrategy     = "lockfile"
	BundlerListLockStrategy = "list-lockfile"
)

var RubyAnalyzer = module.AnalyzerV2{
	Name:         AnalyzerName,
	DiscoverFunc: NewDiscover,
	Strategies: module.Strategies{
		Named: map[module.StrategyName]module.Strategy{
			BundlerListStrategy:     analyzeBundlerList,
			BundlerLockStrategy:     analyzeBundlerLockfile,
			BundlerListLockStrategy: analyzeBundlerListLockfile,
		},
		Optimal: []module.StrategyName{BundlerListLockStrategy},
		SortedNames: []module.StrategyName{
			BundlerListLockStrategy,
			BundlerLockStrategy,
			BundlerListStrategy,
		},
	},
}

func NewDiscover(dir module.Filepath) (map[module.Filepath]module.DiscoveredStrategies, *errors.Error) {
	log.WithField("dir", dir).Debug("discovering modules")
	modules := make(map[module.Filepath]module.DiscoveredStrategies)
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		addStrategy := func(name module.StrategyName) {
			moduleDir := filepath.Dir(path)
			current, ok := modules[moduleDir]
			if !ok {
				current = make(module.DiscoveredStrategies)
			}
			current[name] = info.Name()
			modules[moduleDir] = current
		}

		if !info.IsDir() && info.Name() == "Gemfile.lock" {
			addStrategy(BundlerListLockStrategy)
			addStrategy(BundlerListStrategy)
			addStrategy(BundlerLockStrategy)
		}

		if !info.IsDir() && info.Name() == "Gemfile" {
			addStrategy(BundlerListStrategy)
		}

		return nil
	})

	if err != nil {
		return nil, errors.UnknownError(err, "could not find NodeJS projects")
	}

	return modules, nil
}

func analyzeBundlerListLockfile(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	bundlerCmd, err := bundler.New()
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}
	depGraph, err := bundlerCmd.ListLockfileGraph(filepath.Join(dir, "Gemfile.lock"))
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}

	return depGraph, nil
}

func analyzeBundlerList(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	bundlerCmd, err := bundler.New()
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}
	depGraph, err := bundlerCmd.ListGraph()
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}

	return depGraph, nil
}

func analyzeBundlerLockfile(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	depGraph, err := bundler.LockfileGraph(filepath.Join(dir, "Gemfile.lock"))
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}

	return depGraph, nil
}
