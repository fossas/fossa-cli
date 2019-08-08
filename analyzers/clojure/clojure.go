package clojure

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/leiningen"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Module  module.Module
	Options Options
	Lein    leiningen.Output
}

type Options struct {
	Cmd string `mapstructure:"cmd"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Debugf("%#v", m.Options)

	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	cmd := options.Cmd
	if cmd == "" {
		cmd, err = leiningen.ValidBinary(m.Dir)
		if err != nil {
			log.Warnf("A clojure project has been found at %s, but `lein` could not be found. Ensure that `lein` can be run or set the `FOSSA_LEIN_CMD` environment variable. Associated error: %+v", m.Dir, err)
		}
	}

	analyzer := Analyzer{
		Module:  m,
		Options: options,
		Lein:    leiningen.ShellOutput(cmd, m.Dir),
	}
	return &analyzer, nil

}

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
		}

		if !info.IsDir() && info.Name() == "project.clj" {
			log.WithFields(log.Fields{
				"path": path,
				"name": path,
			}).Debug("found Clojure module")
			relPath, err := filepath.Rel(dir, path)
			if err != nil {
				return errors.Wrap(err, "error discovering rust modules")
			}
			modules = append(modules, module.Module{
				Name:        path,
				Type:        pkg.Clojure,
				BuildTarget: filepath.Base(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}
		return nil
	})
	if err != nil {
		return nil, errors.Wrap(err, "error trying to find clojure packages:")
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
	log.Debugf("Running Clojure analysis: %#v", a.Module)

	// 1. Check for a set strategy.

	// 2. Use the lein command to find a dependency graph.
	dependencies, err := a.Lein.DependencyGraph(a.Module.BuildTarget)
	if err == nil && len(dependencies.Direct) > 0 {
		return dependencies, nil
	}
	// log.Warnf("Falling back to file parsing %s", err)
	// fmt.Println("\n\ninbetween errors\n\n")

	// 3. Parse `project.clj` as best as we can.
	dependenciesFile, fileError := leiningen.ProjectFile(a.Module.Dir, a.Module.BuildTarget)
	if fileError == nil && len(dependenciesFile.Direct) > 0 {
		return dependenciesFile, nil
	}

	//combine troubleshooting errors
	return graph.Deps{}, err
}
