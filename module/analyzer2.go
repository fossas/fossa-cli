package module

import (
	"path/filepath"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
)

type AnalyzerV2 struct {
	Name         string
	DiscoverFunc DiscoverFuncV2
	Strategies   Strategies
}

// The completed analysis by an analyzer
type Analysis struct {
	AnalyzerName string
	Graphs       []TaggedGraph
}

// A discoverfunc takes a root directory to scan and produces a map of
// relative filepaths to discovered strategies
//
// NB: we may discover overlapping strategies for the same module
// For example: if we find package.json and node_modules, we're going to
// discover two `npm ls` strategies
//
// We arbitrarily choose one file to use as the target of that strategy
type DiscoverFuncV2 func(Filepath) (map[Filepath]DiscoveredStrategies, *errors.Error) // Map from 'module' roots to discovered strategies

type Filepath = string
type StrategyName = string

// A strategy produces a dependency graph, with two paths relative to the root
// as arguments:
//
// - The directory of the module. This is usually the parent directory of the
//   "target"
// - The "target" -- usually the file (or directory) that caused the invocation
//   of this strategy
type Strategy func(dir Filepath, target Filepath) (graph.Deps, *errors.Error)

type Strategies struct {
	// Map of strategy names to functions
	Named map[StrategyName]Strategy
	// The order in which to run strategies -- the first strategy to succeed is elected
	// as the "Best" dependency graph
	SortedNames []StrategyName
	// The set of optimal strategies. This will be used for warnings
	Optimal []StrategyName
}

// The filepath is the relative filepath from the root of the module
type DiscoveredStrategies = map[StrategyName]Filepath

type TaggedGraph struct {
	Strategy StrategyName
	// The file that led to this analysis, most often the file
	// analyzed by the relevant strategy
	File  Filepath
	Graph graph.Deps
}

func (a AnalyzerV2) ScanModule(folder Filepath, strategies DiscoveredStrategies) ([]TaggedGraph, *errors.Error) {
	var results []TaggedGraph
	var optimalTroubleshooting []string
	var allTroubleshooting []string

	for _, name := range a.Strategies.SortedNames {
		discovered, ok := strategies[name]
		if !ok {
			continue
		}

		targetFile := filepath.Join(folder, strategies[name])

		result, err := a.Strategies.Named[name](folder, targetFile)
		if err != nil {
			if find(name, a.Strategies.Optimal) {
				optimalTroubleshooting = append(optimalTroubleshooting, err.Troubleshooting)
			}
			allTroubleshooting = append(allTroubleshooting, err.Troubleshooting)
			log.Debugf(`%s: Error when running "%s" strategy on %s: %s`, a.Name, name, folder, err.Error())
			continue
		}

		results = append(results, TaggedGraph{
			File:     targetFile,
			Strategy: discovered,
			Graph:    result,
		})
	}

	var troubleshooting string
	if len(optimalTroubleshooting) > 0 {
		// TODO: we need a better method for this, maybe in the errors package -- to combine a bunch of errors into a useful one
		troubleshooting = strings.Join(optimalTroubleshooting, " OR ")
	} else {
		troubleshooting = strings.Join(allTroubleshooting, " OR ")
	}

	if len(results) == 0 {
		return nil, &errors.Error{
			// TODO: link
			Type:            errors.Unknown,
			Message:         "All strategies failed",
			Troubleshooting: troubleshooting,
		}
	}

	if !find(results[0].Strategy, a.Strategies.Optimal) && len(optimalTroubleshooting) > 0 {
		// TODO: better messages
		log.Warnf("Failed to run optimal strategies. This may produce worse or incomplete dependency graphs")
		log.Warnf("TROUBLESHOOTING: %s", strings.Join(optimalTroubleshooting, " OR "))
	}

	return results, nil
}

func find(elem string, elems []string) bool {
	for _, e := range elems {
		if e == elem {
			return true
		}
	}
	return false
}
