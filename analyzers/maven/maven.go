// Package maven implements Maven analysis.
//
// A `BuildTarget` for Maven is either a Maven project ID (groupId:artifactId), or a path to a directory in
// which there is a "pom.xml" file, or a path to a POM file.
package maven

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

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
	// Strategy can be "pom-file", "maven-tree", or empty.
	Strategy string `mapstructure:"strategy"`
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
	if err != nil {
		log.Warnf("Could not find Maven binary: %s", err.Error())
	}

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
	checked := make(map[string]bool)

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

			submodules, err := maven.Modules(filepath.Join(path, "pom.xml"), path, checked)
			if err != nil {
				log.WithError(err).Debugf("could not get modules at path %s", path)
				return err
			}

			for _, m := range submodules {
				modules = append(modules, module.Module{
					Name:        m.Name,
					Type:        pkg.Maven,
					BuildTarget: m.Target,
					Dir:         m.Dir,
				})
			}
			// Continue recursing because there may be modules that are not declared under the current module.
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

// IsBuilt checks whether `mvn dependency:list` returns without error.
func (a *Analyzer) IsBuilt() (bool, error) {
	output, err := a.Maven.DependencyList(a.Module.Dir, a.Module.BuildTarget)
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

	switch a.Options.Strategy {
	case "pom-file":
		return maven.PomFileGraph(a.Module.BuildTarget, a.Module.Dir)
	case "maven-tree":
		return a.Maven.DependencyTree(a.Module.Dir, a.Module.BuildTarget)
	default:
		if a.Options.Command != "" {
			output, _, err := exec.Shell(exec.Cmd{
				Command: a.Options.Command,
			})
			if err != nil {
				// Because this was a custom shell command, we do not fall back to any other strategies.
				return graph.Deps{}, err
			}
			return maven.ParseDependencyTree(output)
		}

		deps, err := a.Maven.DependencyTree(a.Module.Dir, a.Module.BuildTarget)
		if err != nil {
			log.Warnf(
				"Could not use Maven to determine dependencies for %q: %v. Falling back to use manifest file.",
				a.Module.Name,
				err,
			)
		} else if len(deps.Direct) == 0 {
			log.Warnf("Maven did not find dependencies for %q. Falling back to use manifest file.",
				a.Module.Name)
		} else {
			return deps, nil
		}

		return maven.PomFileGraph(a.Module.BuildTarget, a.Module.Dir)
	}
}
