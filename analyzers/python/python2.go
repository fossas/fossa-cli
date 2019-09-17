package python

import (
	"os"
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/pip"
	"github.com/fossas/fossa-cli/buildtools/pipenv"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
)

const (
	AnalyzerName            = "python"

	SetupPyStrategy         = "setuptools"
	PipStrategy             = "pip"
	PipEnvStrategy          = "pipenv"
	PipDepTreeStrategy      = "deptree"
	RequirementsTxtStrategy = "requirements"
)

var PythonAnalyzer = module.AnalyzerV2{
	Name:         AnalyzerName,
	DiscoverFunc: NewDiscover,
	Strategies:   module.Strategies{
		Named: map[module.StrategyName]module.Strategy{
			SetupPyStrategy: AnalyzeSetupPy,
			PipStrategy: AnalyzePip,
			PipEnvStrategy: AnalyzePipEnv,
			PipDepTreeStrategy: AnalyzePipDepTree,
			RequirementsTxtStrategy: AnalyzeRequirementsTxt,
		},
		SortedNames: []module.StrategyName{PipEnvStrategy, PipDepTreeStrategy, PipStrategy, RequirementsTxtStrategy, SetupPyStrategy},
		Optimal:     []module.StrategyName{PipEnvStrategy, PipDepTreeStrategy, PipStrategy},
	},
}

func NewDiscover(dir module.Filepath) (map[module.Filepath]module.DiscoveredStrategies, *errors.Error) {
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

		if !info.IsDir() && info.Name() == "setup.py" {
			addStrategy(SetupPyStrategy)
			addStrategy(PipStrategy)
			addStrategy(PipEnvStrategy)
			addStrategy(PipDepTreeStrategy)
		}

		if !info.IsDir() && info.Name() == "requirements.txt" {
			addStrategy(RequirementsTxtStrategy)
			addStrategy(PipStrategy)
			addStrategy(PipEnvStrategy)
			addStrategy(PipDepTreeStrategy)
		}

		return nil
	})

	if err != nil {
		return nil, errors.UnknownError(err, "could not find Python projects")
	}

	return modules, nil
}

func AnalyzeSetupPy(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	reqs, err := pip.FromSetupPy(target)
	if err != nil {
		return graph.Deps{}, err
	}
	return requirementsToDeps(reqs), nil
}

func AnalyzePip(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	pip := pip.New()
	reqs, err := pip.List(dir)
	if err != nil {
		return graph.Deps{}, err
	}
	imports := FromRequirements(reqs)
	return graph.Deps{
		Direct:     imports,
		Transitive: fromImports(imports),
	}, nil
}

func AnalyzePipEnv(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	penv := pipenv.New(dir)
	depGraph, err := penv.Deps()
	return depGraph, errors.UnknownError(err, "Couldn't analyze with pipenv")
}

func AnalyzePipDepTree(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	pip := pip.New()
	tree, err := pip.DepTree(dir)
	if err != nil {
		return graph.Deps{}, err
	}
	imports, deps := FromTree(tree)
	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func AnalyzeRequirementsTxt(dir module.Filepath, target module.Filepath) (graph.Deps, *errors.Error) {
	reqs, err := pip.FromFile(target)
	if err != nil {
		return graph.Deps{}, err
	}
	return requirementsToDeps(reqs), nil
}
