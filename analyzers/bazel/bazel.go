package bazel

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/buildtools/bazel"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer defines a Buck analyzer.
type Analyzer struct {
	Module module.Module
	Shell  bazel.Shell
}

// New constructs a new Buck analyzer from a module.
func New(module module.Module) (*Analyzer, error) {
	cmd, _, err := exec.Which("--version", os.Getenv("FOSSA_BAZEL"), "bazel")
	if err != nil {
		return nil, err
	}

	return &Analyzer{
		Module: module,
		Shell:  bazel.ShellOutput(cmd, module.Dir),
	}, nil
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

// Analyze analyzes a bazel build target and its dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	// return bazel.Deps(filepath.Join(a.Module.Dir, a.Module.BuildTarget), true)
	return a.Shell.Command(a.Module.BuildTarget, true)
}

// Discover is used to operate Discovery with a custom `bazel` command.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	modules := []module.Module{}
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
			return err
		}

		// if !info.IsDir() && (info.Name() == "BUILD" || info.Name() == "BUILD.bazel") {
		if !info.IsDir() && info.Name() == "WORKSPACE" {
			moduleName := filepath.Base(path)

			log.WithFields(log.Fields{
				"path": path,
				"name": moduleName,
			}).Debug("constructing Bazel module")
			relPath, err := filepath.Rel(dir, path)
			if err != nil {
				return err
			}

			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Bazel,
				BuildTarget: moduleName,
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Bazel package manifests: %s", err.Error())
	}

	return modules, nil
}
