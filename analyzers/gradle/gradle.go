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
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Module  module.Module
	Options Options
	Input   gradle.Input
}

type Options struct {
	Cmd               string `mapstructure:"cmd"`
	Task              string `mapstructure:"task"`
	Online            bool   `mapstructure:"online"`
	AllSubmodules     bool   `mapstructure:"all-submodules"`
	AllConfigurations bool   `mapstructure:"all-configurations"`
	// TODO: These are temporary until v2 configuration files (with proper BuildTarget) are implemented.
	Project       string `mapstructure:"project"`
	Configuration string `mapstructure:"configuration"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Debugf("%#v", m.Options)
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	binary := options.Cmd
	if binary == "" {
		binary, err = gradle.ValidBinary(m.Dir)
		if err != nil {
			log.Warnf("A build.gradle file has been found at %s, but Gradle could not be found. Ensure that Fossa can access `gradle`, `gradlew`, `gradlew.bat`, or set the `FOSSA_GRADLE_CMD` environment variable. Error: %s", m.Dir, err.Error())
		}
	}

	shellInput := gradle.NewShellInput(binary, m.Dir, options.Online)
	analyzer := Analyzer{
		Module:  m,
		Options: options,
		Input:   shellInput,
	}

	log.Debugf("Initialized Gradle analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover searches for `build.gradle` files and creates a module for each
// `*:dependencies` task in the output of `gradle tasks`.
//
// TODO: use the output of `gradle projects` and try `gradle
// <project>:dependencies` for each project?
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	return DiscoverWithCommand(dir, options, gradle.Cmd)
}

func DiscoverWithCommand(dir string, options map[string]interface{}, command func(string, ...string) (string, error)) ([]module.Module, error) {
	log.WithField("dir", dir).Debug("discovering gradle modules")
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking filepath discovering gradle modules")
			return err
		}

		if info.IsDir() {
			ok, err := files.Exists(path, "build.gradle")
			if err != nil {
				return err
			}
			if !ok {
				return nil
			}

			name := filepath.Base(path)
			bin, err := gradle.ValidBinary(path)
			if err != nil {
				return err
			}
			s := gradle.ShellCommand{
				Binary: bin,
				Dir:    path,
				Cmd:    command,
			}
			projects, err := s.DependencyTasks()
			if err != nil {
				return filepath.SkipDir
			}

			for _, project := range projects {
				modules = append(modules, module.Module{
					Name:        filepath.Join(name, project),
					Type:        pkg.Gradle,
					BuildTarget: project + ":",
					Dir:         path,
				})
			}
			if len(projects) == 0 {
				modules = append(modules, module.Module{
					Name:        filepath.Base(path),
					Type:        pkg.Gradle,
					BuildTarget: ":",
					Dir:         path,
				})
			}
			// Don't continue recursing, because anything else is probably a
			// subproject.
			return filepath.SkipDir
		}
		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find Gradle projects")
	}

	return modules, nil
}

func (a *Analyzer) Clean() error {
	return nil
}

func (a *Analyzer) Build() error {
	return nil
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Debugf("Running Gradle analysis: %#v", a.Module)

	version := config.Version()
	// cli v0.7.21 or earlier. This version takes a build target in the format of <project>:<configuration>.
	if version > 0 && version <= 1 {
		return parseModuleV1(a)
	}
	// cli v0.7.22 and later does not split project on `:` to account for deep sub-projects.
	return parseModuleV2(a)
}

var defaultConfigurations = []string{"compile", "api", "implementation", "compileDependenciesMetadata", "apiDependenciesMetadata", "implementationDependenciesMetadata"}

func parseModuleV1(a *Analyzer) (graph.Deps, error) {
	var configurations []string
	var depsByConfig map[string]graph.Deps
	var err error
	targets := strings.Split(a.Module.BuildTarget, ":")

	if a.Options.AllSubmodules {
		submodules, err := a.Input.DependencyTasks()
		if err != nil {
			return graph.Deps{}, err
		}
		depsByConfig, err = gradle.MergeProjectsDependencies(a.Input, submodules)
		if err != nil {
			return graph.Deps{}, err
		}
	} else if a.Options.Task != "" {
		depsByConfig, err = a.Input.ProjectDependencies(strings.Split(a.Options.Task, " ")...)
		if err != nil {
			return graph.Deps{}, err
		}
	} else {
		project := a.Options.Project
		if project == "" {
			project = targets[0]
		}
		depsByConfig, err = gradle.Dependencies(project, a.Input)
		if err != nil {
			return graph.Deps{}, err
		}
	}

	if a.Options.Configuration != "" {
		configurations = strings.Split(a.Options.Configuration, ",")
	} else if len(targets) > 1 && targets[1] != "" {
		configurations = strings.Split(targets[1], ",")
	} else if a.Options.AllConfigurations {
		for config := range depsByConfig {
			configurations = append(configurations, config)
		}
	} else {
		configurations = defaultConfigurations
	}

	merged := graph.Deps{
		Direct:     nil,
		Transitive: make(map[pkg.ID]pkg.Package),
	}
	for _, config := range configurations {
		merged = mergeGraphs(merged, depsByConfig[config])
	}
	return merged, nil
}

func parseModuleV2(a *Analyzer) (graph.Deps, error) {
	var configurations []string
	var depsByConfig map[string]graph.Deps
	var err error

	if a.Options.AllSubmodules {
		submodules, err := a.Input.DependencyTasks()
		if err != nil {
			return graph.Deps{}, err
		}
		depsByConfig, err = gradle.MergeProjectsDependencies(a.Input, submodules)
		if err != nil {
			return graph.Deps{}, err
		}
	} else if a.Options.Task != "" {
		depsByConfig, err = a.Input.ProjectDependencies(strings.Split(a.Options.Task, " ")...)
		if err != nil {
			return graph.Deps{}, err
		}
	} else {
		project := a.Options.Project
		if project == "" {
			project = a.Module.BuildTarget
		}
		depsByConfig, err = gradle.Dependencies(project, a.Input)
		if err != nil {
			return graph.Deps{}, err
		}
	}

	if a.Options.Configuration != "" {
		configurations = strings.Split(a.Options.Configuration, ",")
	} else if a.Options.AllConfigurations {
		for config := range depsByConfig {
			configurations = append(configurations, config)
		}
	} else {
		configurations = defaultConfigurations
	}

	merged := graph.Deps{
		Direct:     nil,
		Transitive: make(map[pkg.ID]pkg.Package),
	}
	for _, config := range configurations {
		merged = mergeGraphs(merged, depsByConfig[config])
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
