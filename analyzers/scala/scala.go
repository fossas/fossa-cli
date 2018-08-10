// Package scala implements Scala analysis.
//
// A `BuildTarget` for scala is project:configuration.
package scala

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/sbt"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	SBTCmd     string
	SBTVersion string

	JavaCmd     string
	JavaVersion string

	SBT     sbt.SBT
	Module  module.Module
	Options Options
}

type Options struct{}

func New(m module.Module) (*Analyzer, error) {
	// Set Java context variables
	javaCmd, javaVersion, err := exec.Which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		// Is JAVA_HOME set?
		// Is $FOSSA_JAVA_BINARY set?
		log.WithError(err).Warn("could not find Java binary")
	}

	// Set SBT context variables
	sbtCmd, sbtVersion, err := exec.Which("-no-colors about", os.Getenv("SBT_BINARY"), "sbt")
	if err != nil {
		// Is FOSSA_SBT_BINARY set?
		log.WithError(err).Warn("could not find SBT binary")
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("decoded SBT analyzer options")

	analyzer := Analyzer{
		JavaCmd:     javaCmd,
		JavaVersion: javaVersion,

		SBTCmd:     sbtCmd,
		SBTVersion: sbtVersion,

		SBT: sbt.SBT{
			Bin: sbtCmd,
		},
		Module:  m,
		Options: options,
	}

	log.WithField("analyzer", analyzer).Debug("constructed SBT analyzer")
	return &analyzer, nil
}

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	log.WithField("dir", dir).Debug("discovering modules")

	// Construct SBT instance (for listing projects).
	sbtCmd, _, err := exec.Which("-no-colors about", os.Getenv("SBT_BINARY"), "sbt")
	if err != nil {
		return nil, nil
	}
	sbt := sbt.SBT{
		Bin: sbtCmd,
	}

	var modules []module.Module
	err = filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		log.WithField("path", path).Debug("discovering modules")

		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		if info.IsDir() {
			log.Debug("path is folder")
			ok, err := files.Exists(path, "build.sbt")
			if err != nil {
				return err
			}
			if !ok {
				return nil
			}
			log.Debug("Path has build.sbt")

			dir := filepath.Dir(path)
			projects, err := sbt.Projects(dir)
			if err != nil {
				log.WithError(err).Debug("could not get modules at path")
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
			log.WithField("path", path).Debug("skipping")
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

func (a *Analyzer) Clean() error {
	project, configuration := ParseTarget(a.Module.BuildTarget)
	return a.SBT.Clean(a.Module.Dir, project, configuration)
}

func (a *Analyzer) Build() error {
	project, configuration := ParseTarget(a.Module.BuildTarget)
	return a.SBT.Compile(a.Module.Dir, project, configuration)
}

// IsBuilt checks whether `mvn dependency:list` produces an error.
func (a *Analyzer) IsBuilt() (bool, error) {
	project, configuration := ParseTarget(a.Module.BuildTarget)
	output, err := a.SBT.DependencyList(a.Module.Dir, project, configuration)
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

	project, configuration := ParseTarget(a.Module.BuildTarget)
	imports, deps, err := a.SBT.DependencyTree(a.Module.Dir, project, configuration)
	if err != nil {
		return graph.Deps{}, err
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func ParseTarget(target string) (project string, configuration string) {
	splits := strings.Split(target, ":")
	project = splits[0]
	configuration = splits[1]
	return project, configuration
}
