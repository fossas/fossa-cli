package module

import (
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
)

type AnalyzerV2 struct {
	Name         string
	DiscoverFunc DiscoverFuncV2
	Strategies   Strategies
}

type AnalyzerV2Output map[Filepath][]Analysis

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
type DiscoverFuncV2 func(dir Filepath) (map[Filepath]DiscoveredStrategies, *errors.Error) // Map from 'module' roots to discovered strategies

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

// FilepathStrategies maps strategies to the filepath they were found in.
type FilepathStrategies map[Filepath]DiscoveredStrategies

// AddStrategy adds a strategy with filepath to the FilepathStrategies.
func (fileStrategies FilepathStrategies) AddStrategy(info os.FileInfo, path string, name StrategyName) {
	current, ok := fileStrategies[path]
	if !ok {
		current = make(DiscoveredStrategies)
	}
	current[name] = info.Name()
	fileStrategies[path] = current
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

func (a AnalyzerV2) ScanModule(startJob func() error, endJob func(), progress display.ProgressTracker, folder Filepath, strategies DiscoveredStrategies) ([]TaggedGraph, *errors.Error) {
	var wg sync.WaitGroup

	graphs := make(chan TaggedGraph)
	allErrs := make(chan *errors.Error, len(strategies))
	optimalErrs := make(chan *errors.Error, len(strategies))

	for name, relPath := range strategies {
		progress.AddTasks(1)
		wg.Add(1)
		go func(name StrategyName, relPath Filepath) {
			defer wg.Done()
			defer progress.AddCompleted(1)

			if err := startJob(); err != nil {
				// TODO: better errors
				log.Debugf("%s: Failed to acquire semaphore for strategy \"%s\": %s", a.Name, name, err.Error())
				return
			}

			defer endJob()

			result, err := a.Strategies.Named[name](folder, filepath.Join(folder, relPath))
			if err != nil {
				if find(name, a.Strategies.Optimal) {
					optimalErrs <- err
				}
				allErrs <- err
				return
			}

			graphs <- TaggedGraph{
				Strategy: name,
				File:     filepath.Join(folder, relPath),
				Graph:    result,
			}
		}(name, relPath) // TODO: break out into its own function?
	}

	go func() {
		wg.Wait()
		close(graphs)
		close(allErrs)
		close(optimalErrs)
	}()

	var results []TaggedGraph

	for graph := range graphs {
		results = append(results, graph)
	}

	sort.Slice(results, func(i, j int) bool {
		// TODO: this has explosive complexity
		return find(results[i].Strategy, a.Strategies.Optimal)
	})

	if len(results) == 0 {
		var troubleshooting []string
		for allErr := range allErrs {
			troubleshooting = append(troubleshooting, allErr.Troubleshooting)
		}

		return nil, &errors.Error{
			// TODO: link
			Type:            errors.Unknown,
			Message:         "All strategies failed",
			Troubleshooting: strings.Join(troubleshooting, " OR "),
		}
	}

	if !find(results[0].Strategy, a.Strategies.Optimal) && len(optimalErrs) > 0 {
		var troubleshooting []string
		for optimalErr := range optimalErrs {
			troubleshooting = append(troubleshooting, optimalErr.Troubleshooting)
		}

		// TODO: better messages
		log.Warnf("Failed to run optimal strategies. This may produce worse or incomplete dependency graphs")
		log.Warnf("TROUBLESHOOTING: %s", strings.Join(troubleshooting, " OR "))
	}

	// TODO(parallel): error reporting

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
