package module

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type AnalyzerV2 struct {
	DiscoverFunc DiscoverFuncV2
	Strategies   Strategies
}

// A discoverfunc takes a root directory to scan and produces a map of
// relative filepaths to discovered strategies
type DiscoverFuncV2 func(Filepath) (map[Filepath][]DiscoveredStrategy, *errors.Error) // Map from 'module' roots to discovered strategies

type Filepath = string
type StrategyName = string
type Strategy func(Filepath) (graph.Deps, *errors.Error)

type DiscoveredStrategy struct {
	Name StrategyName
	// The relative filepath from the 'module' root to the file that triggered the analysis
	// Often the same as the file being analyzed by this strategy
	// Will be used to 'tag' the resulting dependency graph
	RelTarget Filepath
}

type Strategies struct {
	// Map of strategy names to functions
	Named map[StrategyName]Strategy
	// The order in which to run strategies -- the first strategy to succeed is elected
	// as the "Best" dependency graph
	SortedNames []StrategyName
	// The set of optimal strategies. This will be used for warnings
	Optimal []StrategyName
}

type TaggedGraph struct {
	Tags  map[string]string // TODO: concrete tags
	Graph graph.Deps
}

func (a AnalyzerV2) AnalyzeV2() (map[Filepath][]TaggedGraph, *errors.Error) {
	strategies, err := a.DiscoverFunc(".") // TODO: hardcoded "."
	if err != nil {
		return nil, err // TODO: this happens when discovery fails. should probably ignore the error or warn
	}

	modules := make(map[Filepath][]TaggedGraph)
	for folder := range strategies {
		scanned, err := a.scanModule(folder, strategies[folder])
		if err != nil {
			return nil, err // TODO: this happens when an individual strategy fails
		}

		modules[folder] = scanned
	}

	return modules, nil
}

func (a AnalyzerV2) scanModule(folder Filepath, strategies []DiscoveredStrategy) ([]TaggedGraph, *errors.Error) {

	// NB: we may have discovered overlapping strategies for the same module
	// For example: if we find package.json and node_modules, we're going to
	// discover two `npm ls` strategies
	//
	// We arbitrarily choose one DiscoveredStrategy per strategy type
	strategiesByType := make(map[StrategyName]DiscoveredStrategy)

	for _, strategy := range strategies {
		strategiesByType[strategy.Name] = strategy
	}

	// TODO: strategy name validation / sanity check

	var results []TaggedGraph
	for _, name := range a.Strategies.SortedNames {
		discovered, ok := strategiesByType[name]
		if !ok {
			continue
		}

		result, err := a.Strategies.Named[name](filepath.Join(folder, discovered.RelTarget))
		if err != nil {
			// TODO: this err is when an individual strategy fails
			continue
		}

		results = append(results, TaggedGraph{
			Tags: map[string]string{
				"target": discovered.RelTarget, // TODO: other tags?
			},
			Graph: result,
		})
	}

	// TODO: warning about optimal strategies
	return results, nil
}

// TODO: remove this -- temporary shim
func ToModules(analyzed map[Filepath][]TaggedGraph) []Module {
	var modules []Module

	for path := range analyzed {
		graphs := analyzed[path]

		// NB: this removes other graphs
		elected := graph.Deps{}
		if len(graphs) > 0 {
			// NB: this eliminates tags
			elected = graphs[0].Graph
		}

		modules = append(modules, Module{
			Name:         "NAME",
			Type:         pkg.NodeJS, // TODO?
			IsExecutable: false,
			Ignore:       false,
			BuildTarget:  "BUILDTARGET",
			Dir:          path,
			Options:      nil,
			Imports:      elected.Direct,
			Deps:         elected.Transitive,
		})
	}

	return modules
}
