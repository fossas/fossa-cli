// Package gradle implements analyzers for Gradle.
//
// A `BuildTarget` in Gradle is `$PROJECT:$CONFIGURATION`, where the Gradle
// module would list its dependencies by running `gradle $PROJECT:dependencies
// --configuration=$CONFIGURATION`. The directory of the `build.gradle` file is
// specified by `Dir`.
package gradle

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

const (
	AnalyzerName = "gradle"

	DependenciesCmd = "dependencies"
)

var GradleAnalyzer = module.AnalyzerV2{
	Name:         AnalyzerName,
	DiscoverFunc: NewDiscover,
	Strategies: module.Strategies{
		Named: map[module.StrategyName]module.Strategy{
			// DependenciesCmd: AnalyzeYarnCmd,
		},
		Optimal: []module.StrategyName{DependenciesCmd},
		SortedNames: []module.StrategyName{
			DependenciesCmd,
		},
	},
}

// type Options struct {
// 	Online            bool   `mapstructure:"online"`
// }

func NewDiscover(dir module.Filepath) (map[module.Filepath]module.DiscoveredStrategies, *errors.Error) {
	log.WithField("dir", dir).Debug("discovering gradle modules")
	modules := module.FilepathStrategies{}
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		if !info.IsDir() && info.Name() == "build.gradle" {
			modules.AddStrategy(info, path, DependenciesCmd)
		}

		return nil
	})

	if err != nil {
		return nil, errors.UnknownError(err, "could not find Gradle projects")
	}

	return modules, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	return parseModuleV2(a)
}

// Groups of configurations to pull dependencies from. Later configuration
// groups in this list are used as fallbacks when dependencies aren't found for
// the current group.
var defaultConfigurationGroups = [][]string{
	{"compileClasspath", "runtimeClasspath"},
	{"compile", "api", "implementation", "compileDependenciesMetadata", "apiDependenciesMetadata", "implementationDependenciesMetadata"},
}

func parseModuleV2(a *Analyzer) (graph.Deps, error) {
	var configurationGroups [][]string
	var depsByConfig map[string]graph.Deps
	var err error

	submodules, err := a.Input.DependencyTasks()
	if err != nil {
		return graph.Deps{}, err
	}
	depsByConfig, err = gradle.MergeProjectsDependencies(a.Input, submodules)
	if err != nil {
		return graph.Deps{}, err
	}

	if a.Options.Configuration != "" {
		configurationGroups = [][]string{strings.Split(a.Options.Configuration, ",")}
	} else if a.Options.AllConfigurations {
		var configurations []string
		for config := range depsByConfig {
			configurations = append(configurations, config)
		}
		configurationGroups = [][]string{configurations}
	} else {
		configurationGroups = defaultConfigurationGroups
	}

	merged := graph.Deps{
		Direct:     nil,
		Transitive: make(map[pkg.ID]pkg.Package),
	}
	for _, group := range configurationGroups {
		for _, config := range group {
			merged = mergeGraphs(merged, depsByConfig[config])
		}

		if len(merged.Direct) > 0 {
			break
		}
	}
	return merged, nil
}

func mergeGraphs(gs ...graph.Deps) graph.Deps {
	merged := graph.Deps{
		Direct:     nil,
		Transitive: make(map[pkg.ID]pkg.Package),
	}

	importSet := make(map[pkg.Import]bool)
	for _, g := range gs {
		for _, i := range g.Direct {
			importSet[i] = true
		}
		for id, dep := range g.Transitive {
			merged.Transitive[id] = dep
		}
	}
	for i := range importSet {
		merged.Direct = append(merged.Direct, i)
	}

	return merged
}
