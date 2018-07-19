// Package scala implements Scala analysis.
//
// A `BuildTarget` for scala is project:configuration.
package scala

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/sbt"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	SBTCmd     string
	SBTVersion string

	JavaCmd     string
	JavaVersion string

	SBT     sbt.SBT
	Options Options
}

type Options struct{}

func New(opts map[string]interface{}) (*Analyzer, error) {
	// Set Java context variables
	javaCmd, javaVersion, err := exec.Which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		log.Logger.Warningf("Could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}

	// Set SBT context variables
	sbtCmd, sbtVersion, err := exec.Which("-no-colors about", os.Getenv("SBT_BINARY"), "sbt")
	if err != nil {
		return nil, fmt.Errorf("could not find SBT binary (try setting $SBT_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	analyzer := Analyzer{
		JavaCmd:     javaCmd,
		JavaVersion: javaVersion,

		SBTCmd:     sbtCmd,
		SBTVersion: sbtVersion,

		SBT: sbt.SBT{
			Bin: sbtCmd,
		},
		Options: options,
	}

	log.Logger.Debugf("%#v", analyzer)
	return &analyzer, nil
}

func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	log.Logger.Debugf("%#v", dir)
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		log.Logger.Debugf("Searching path: %#v", path)

		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		if info.IsDir() {
			log.Logger.Debugf("Path is folder")
			ok, err := files.Exists(path, "build.sbt")
			if err != nil {
				return err
			}
			if !ok {
				return nil
			}
			log.Logger.Debugf("Path has build.sbt")

			dir := filepath.Dir(path)
			projects, err := a.SBT.Projects(dir)
			if err != nil {
				log.Logger.Debugf("Modules err: %#v %#v", err.Error(), err)
				return err
			}

			for _, p := range projects {
				modules = append(modules, module.Module{
					Name:        p,
					Type:        pkg.Scala,
					BuildTarget: p + ":compile",
					Dir:         dir,
				})
			}
			log.Logger.Debugf("skipDir: %#v", path)
			// Don't continue recursing, because anything else is probably a
			// subproject.
			return filepath.SkipDir
		}

		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find Scala projects")
	}

	return modules, nil
}

func (a *Analyzer) Clean(m module.Module) error {
	project, configuration := ParseTarget(m.BuildTarget)
	return a.SBT.Clean(m.Dir, project, configuration)
}

func (a *Analyzer) Build(m module.Module) error {
	project, configuration := ParseTarget(m.BuildTarget)
	return a.SBT.Compile(m.Dir, project, configuration)
}

// IsBuilt checks whether `mvn dependency:list` produces an error.
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	project, configuration := ParseTarget(m.BuildTarget)
	output, err := a.SBT.DependencyList(m.Dir, project, configuration)
	if err != nil {
		if strings.Contains(output, "Could not find artifact") {
			return false, nil
		}
		return false, err
	}
	return output != "", nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("%#v", m)

	project, configuration := ParseTarget(m.BuildTarget)
	imports, graph, err := a.SBT.DependencyTree(m.Dir, project, configuration)
	if err != nil {
		return m, err
	}

	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Scala,
				Name:     dep.Name,
				Revision: dep.Version,
			},
		})
	}

	// Set transitive dependencies.
	g := make(map[pkg.ID]pkg.Package)
	for parent, children := range graph {
		id := pkg.ID{
			Type:     pkg.Scala,
			Name:     parent.Name,
			Revision: parent.Version,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Scala,
					Name:     child.Name,
					Revision: child.Version,
				},
			})
		}
		g[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	m.Imports = i
	m.Deps = g
	return m, nil
}

func ParseTarget(target string) (project string, configuration string) {
	splits := strings.Split(target, ":")
	project = splits[0]
	configuration = splits[1]
	return project, configuration
}
