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

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	GradleCmd string

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

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debugf("%#v", opts)

	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		GradleCmd: options.Cmd,
		Options:   options,
	}

	if analyzer.GradleCmd == "" {
		gradle, _, err := exec.Which("-v", os.Getenv("FOSSA_GRADLE_CMD"), "./gradlew", "gradle")
		if err != nil {
			log.Logger.Warningf("Could not find Gradle: %s", err.Error())
		}
		analyzer.GradleCmd = gradle
	}

	log.Logger.Debugf("Initialized Gradle analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover searches for `build.gradle` files and creates a module for each
// `*:dependencies` task in the output of `gradle tasks`.
//
// TODO: use the output of `gradle projects` and try `gradle
// <project>:dependencies` for each project?
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	log.Logger.Debugf("%#v", dir)
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
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
					BuildTarget: dir,
					Dir:         dir,
					Options: map[string]interface{}{
						"project": project,
					},
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

func (a *Analyzer) Clean(m module.Module) error {
	return nil
}

func (a *Analyzer) Build(m module.Module) error {
	return nil
}

func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	return true, nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("Running Gradle analysis: %#v", m)

	// Get packages.
	g := gradle.Gradle{
		Cmd:    a.GradleCmd,
		Dir:    m.Dir,
		Online: a.Options.Online,
	}
	var imports []gradle.Dependency
	var deps map[gradle.Dependency][]gradle.Dependency
	var err error
	if a.Options.Task != "" {
		imports, deps, err = g.DependenciesTask(strings.Split(a.Options.Task, " ")...)
		if err != nil {
			return m, err
		}
	} else if a.Options.Project != "" {
		imports, deps, err = g.Dependencies(a.Options.Project, a.Options.Configuration)
		if err != nil {
			return m, err
		}
	} else {
		targets := strings.Split(m.BuildTarget, ":")
		imports, deps, err = g.Dependencies(targets[0], targets[1])
		if err != nil {
			return m, err
		}
	}

	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Target: dep.Target,
			Resolved: pkg.ID{
				Type:     pkg.Gradle,
				Name:     dep.Name,
				Revision: dep.Resolved,
			},
		})
	}

	// Set transitive dependencies.
	graph := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		id := pkg.ID{
			Type:     pkg.Gradle,
			Name:     parent.Name,
			Revision: parent.Resolved,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.Resolved,
				Resolved: pkg.ID{
					Type:     pkg.Gradle,
					Name:     child.Name,
					Revision: child.Resolved,
				},
			})
		}
		graph[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	m.Imports = i
	m.Deps = graph
	return m, nil
}
