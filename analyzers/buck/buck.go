package buck

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/buck"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer defines a Buck analyzer.
type Analyzer struct {
	Module module.Module
}

// New constructs a new Buck analyzer from a module.
func New(module module.Module) (*Analyzer, error) {
	analyzer := Analyzer{
		Module: module,
	}
	return &analyzer, nil
}

// Clean is not implemented.
func (a *Analyzer) Clean() error {
	return nil
}

// Build is not implemented.
func (a *Analyzer) Build() error {
	return nil
}

// IsBuilt is not implemented.
func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Analyze analyzers a buck package and its dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	return buck.Deps(a.Module.BuildTarget)
}

// Discover finds directories that correspond to Buck projects.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	var moduleList []module.Module

	err := filepath.Walk(dir, func(filename string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("filename", filename).Debug("failed to access path")
			return err
		}

		if !info.IsDir() && (info.Name() == ".buckconfig") {
			moduleDir := filepath.Dir(filename)
			moduleName := filepath.Base(moduleDir)

			log.WithFields(log.Fields{
				"path": filename,
				"name": moduleName,
			}).Debug("constructing Buck module")

			relPath, _ := filepath.Rel(dir, filename)
			if moduleName == "." {
				name, err := os.Getwd()
				if err == nil {
					moduleName = filepath.Base(name)
				}
			}

			moduleList = append(moduleList, module.Module{
				Name:        moduleName,
				Type:        pkg.Buck,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}
		return nil
	})
	if err != nil {
		return nil, errors.Errorf("Could not find Buck config file: %s", err.Error())
	}

	return moduleList, nil
}
