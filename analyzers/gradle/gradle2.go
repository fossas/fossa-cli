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

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/module"
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
			DependenciesCmd: gradle.Deps,
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

		if info.IsDir() {
			buildScripts, err := filepath.Glob(filepath.Join(path, "build.gradle*"))
			if err != nil {
				return err
			}

			if len(buildScripts) == 0 {
				return nil
			}

			modules.AddStrategy(info, path, DependenciesCmd)
			return filepath.SkipDir
		}

		return nil
	})

	if err != nil {
		return nil, errors.UnknownError(err, "could not find Gradle projects")
	}

	return modules, nil
}
