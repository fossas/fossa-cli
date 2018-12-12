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
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	GradleCmd string

	Module  module.Module
	Options Options
}

type Options struct {
	Cmd    string `mapstructure:"cmd"`
	Task   string `mapstructure:"task"`
	Online bool   `mapstructure:"online"`

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

	analyzer := Analyzer{
		GradleCmd: options.Cmd,
		Module:    m,
		Options:   options,
	}

	if analyzer.GradleCmd == "" {
		gradle, _, err := exec.Which("-v", os.Getenv("FOSSA_GRADLE_CMD"), "./gradlew", "gradle")
		if err != nil {
			log.Warnf("Could not find Gradle: %s", err.Error())
		}
		analyzer.GradleCmd = gradle
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
	log.WithField("dir", dir).Debug("discovering modules")
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
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

			dir := filepath.Dir(path)
			name := filepath.Base(dir)
			cmd, err := gradle.Cmd(dir)
			if err != nil {
				return err
			}
			g := gradle.Gradle{
				Cmd: cmd,
				Dir: dir,
			}
			projects, err := g.Projects()
			if err != nil {
				return err
			}
			for _, project := range projects {
				modules = append(modules, module.Module{
					Name:        filepath.Join(name, project),
					Type:        pkg.Gradle,
					BuildTarget: project + ":",
					Dir:         dir,
				})
			}
			if len(projects) == 0 {
				modules = append(modules, module.Module{
					Name:        name,
					Type:        pkg.Gradle,
					BuildTarget: ":",
					Dir:         dir,
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

	// Get packages.
	g := gradle.Gradle{
		Cmd:    a.GradleCmd,
		Dir:    a.Module.Dir,
		Online: a.Options.Online,
	}

	var project string
	var configurations []string
	var depsByConfig map[string]graph.Deps
	var err error
	targets := strings.Split(a.Module.BuildTarget, ":")

	if a.Options.Task != "" {
		depsByConfig, err = g.DependenciesTask(strings.Split(a.Options.Task, " ")...)
		if err != nil {
			return graph.Deps{}, err
		}
	} else {
		if a.Options.Project != "" {
			project = a.Options.Project
		} else {
			project = targets[0]
		}
		depsByConfig, err = g.Dependencies(project)
		if err != nil {
			return graph.Deps{}, err
		}
	}

	defaultConfigurations := []string{"compile", "api", "implementation"}
	if a.Options.Configuration != "" {
		configurations = strings.Split(a.Options.Configuration, ",")
	} else if len(targets) > 1 && targets[1] != "" {
		configurations = strings.Split(targets[1], ",")
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
