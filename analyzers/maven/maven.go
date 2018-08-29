// Package maven implements Maven analysis.
//
// A `BuildTarget` for Maven is the Maven project name.
package maven

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Maven   maven.Maven
	Module  module.Module
	Options Options
}

type Options struct {
	Binary  string `mapstructure:"bin"`
	Command string `mapstructure:"cmd"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Debugf("%#v", m.Options)

	// Decode options.
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	// Get Maven binary.
	mvnBin, _, err := exec.Which("--version", options.Binary, os.Getenv("MAVEN_BINARY"), "mvn")

	analyzer := Analyzer{
		Maven: maven.Maven{
			Cmd: mvnBin,
		},
		Module:  m,
		Options: options,
	}

	log.WithField("analyzer", analyzer).Debug("constructed analyzer")
	return &analyzer, nil
}

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	log.WithField("dir", dir).Debug("discovering modules")
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		if info.IsDir() {
			ok, err := files.Exists(path, "pom.xml")
			if err != nil {
				return err
			}
			if !ok {
				return nil
			}

			submodules, err := maven.Modules(path)
			if err != nil {
				log.WithError(err).Debug("could not get modules at path")
				return err
			}

			for _, m := range submodules {
				modules = append(modules, module.Module{
					Name:        m,
					Type:        pkg.Maven,
					BuildTarget: m,
					Dir:         path,
				})
			}
			log.WithField("path", path).Debug("skipping")
			// Don't continue recursing, because anything else is probably a
			// subproject.
			return filepath.SkipDir
		}

		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find Maven projects")
	}

	return modules, nil
}

func (a *Analyzer) Clean() error {
	return a.Maven.Clean(a.Module.Dir)
}

func (a *Analyzer) Build() error {
	return a.Maven.Compile(a.Module.Dir)
}

// IsBuilt checks whether `mvn dependency:list` produces an error.
func (a *Analyzer) IsBuilt() (bool, error) {
	output, err := a.Maven.DependencyList(a.Module.Dir)
	if err != nil {
		if strings.Contains(output, "Could not find artifact") {
			return false, nil
		}
		return false, err
	}
	return output != "", nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.WithField("module", a.Module).Debug("analyzing module")

	var imports []maven.Dependency
	var deps map[maven.Dependency][]maven.Dependency
	var err error
	if a.Options.Command == "" {
		imports, deps, err = a.Maven.DependencyTree(a.Module.Dir, a.Module.BuildTarget)
	} else {
		var output string
		output, _, err = exec.Shell(exec.Cmd{
			Command: a.Options.Command,
		})
		if err != nil {
			return graph.Deps{}, err
		}
		imports, deps, err = maven.ParseDependencyTree(output)
	}
	if err != nil {
		return graph.Deps{}, err
	}

	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Version,
			},
		})
	}

	// Set transitive dependencies.
	g := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		id := pkg.ID{
			Type:     pkg.Maven,
			Name:     parent.Name,
			Revision: parent.Version,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Maven,
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

	return graph.Deps{
		Direct:     i,
		Transitive: g,
	}, nil
}
