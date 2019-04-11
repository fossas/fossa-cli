package haskell

import (
	"fmt"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"os"
	"path/filepath"
	"strings"
)

func NewModule(moduleName string, relativeDir string, strategy Strategy) module.Module {
	return module.Module{
		Name:        moduleName,
		Type:        pkg.Haskell,
		BuildTarget: relativeDir,
		Dir:         relativeDir,
		Options:     map[string]interface{}{"strategy": strategy},
	}
}

func Discover(baseDir string, options map[string]interface{}) ([]module.Module, error) {
	// List of discovered projects. Projects can be defined in
	// `cabal.project` or `stack.yaml` files
	projects := make(map[string]module.Module)
	// List of discovered cabal files. These will be used in the case that:
	// - No projects are found; or
	// - A project doesn't exist in the current or a parent directory
	cabalFiles := make(map[string]module.Module)

	err := filepath.Walk(baseDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("filename", path).Debug("failed to access path")
			return err
		}

		if info.IsDir() {
			return nil
		}

		// TODO: get module name from somewhere
		moduleName     := filepath.Base(path)
		relativeDir, _ := filepath.Rel(baseDir, filepath.Dir(path))

		if info.Name() == "cabal.project" {
			projects[relativeDir] = NewModule(moduleName, relativeDir, CabalInstall)
		} else if info.Name() == "stack.yaml" {
			projects[relativeDir] = NewModule(moduleName, relativeDir, Stack)
		} else if strings.HasSuffix(info.Name(), ".cabal") {
			cabalFiles[relativeDir] = NewModule(moduleName, relativeDir, CabalInstall)
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find Haskell package manifests: %s", err.Error())
	}

	// Prune .cabal files where parent projects are present
	for cabalFilePath := range cabalFiles {
		for projectPath := range projects {
			// TODO: parse cabal.project and stack.yaml instead of using their paths?
			// TODO: tests for this relative filepath thing
			rel, err := filepath.Rel(projectPath, cabalFilePath)
			if err == nil && !strings.Contains(rel, "..") {
				delete(cabalFiles, cabalFilePath)
				break
			}
		}
	}

	var modules []module.Module //make([]module.Module, len(projects) + len(cabalFiles))
	for _, m := range projects {
		modules = append(modules, m)
	}
	for _, m := range cabalFiles {
		modules = append(modules, m)
	}

	return modules, nil
}
