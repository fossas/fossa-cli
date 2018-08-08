// Package python provides analysers for Python projects.
//
// A `BuildTarget` in Python is the directory of the Python project, generally
// containing `requirements.txt` or `setup.py`.
package python

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/pip"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	PythonCmd     string
	PythonVersion string

	Pip     pip.Pip
	Module  module.Module
	Options Options
}

type Options struct {
	Strategy         string `mapstructure:"strategy"`
	RequirementsPath string `mapstructure:"requirements"`
	VirtualEnv       string `mapstructure:"venv"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debug("%#v", m.Options)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	// Construct analyzer.
	pythonCmd, pythonVersion, err := exec.Which("--version", os.Getenv("FOSSA_PYTHON_CMD"), "python", "python3", "python2.7")
	if err != nil {
		return nil, err
	}
	// TODO: this should be fatal depending on the configured strategy.
	pipCmd, _, err := exec.Which("--version", os.Getenv("FOSSA_PIP_CMD"), "pip3", "pip")
	if err != nil {
		log.Logger.Warningf("`pip` command not detected")
	}
	return &Analyzer{
		PythonCmd:     pythonCmd,
		PythonVersion: pythonVersion,

		Pip: pip.Pip{
			Cmd:       pipCmd,
			PythonCmd: pythonCmd,
		},
		Module:  m,
		Options: options,
	}, nil
}

// Discover constructs modules in all directories with a `requirements.txt` or
// `setup.py`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	// A map of directory to module. This is to avoid multiple modules in one
	// directory e.g. if we find _both_ a `requirements.txt` and `setup.py`.
	modules := make(map[string]module.Module)

	err := filepath.Walk(dir, func(filename string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", filename, err.Error())
			return err
		}

		if !info.IsDir() && (info.Name() == "requirements.txt" || info.Name() == "setup.py") {
			moduleDir := filepath.Dir(filename)
			_, ok := modules[moduleDir]
			if ok {
				// We've already constructed a module for this directory.
				return nil
			}

			moduleName := filepath.Base(moduleDir)

			log.Logger.Debugf("Found Python package: %s (%s)", filename, moduleName)
			relPath, _ := filepath.Rel(dir, filename)
			modules[moduleDir] = module.Module{
				Name:        moduleName,
				Type:        pkg.Python,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			}
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Python package manifests: %s", err.Error())
	}

	var moduleList []module.Module
	for _, m := range modules {
		moduleList = append(moduleList, m)
	}
	return moduleList, nil
}

// Clean logs a warning and does nothing for Python.
func (a *Analyzer) Clean() error {
	log.Logger.Warningf("Clean is not implemented for Python")
	return nil
}

func (a *Analyzer) Build() error {
	return a.Pip.Install(a.requirementsFile(a.Module))
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	switch a.Options.Strategy {
	case "deptree":
		tree, err := a.Pip.DepTree()
		if err != nil {
			return graph.Deps{}, err
		}
		imports, deps := FromTree(tree)
		return graph.Deps{
			Direct:     imports,
			Transitive: deps,
		}, nil
	case "pip":
		reqs, err := a.Pip.List()
		if err != nil {
			return graph.Deps{}, err
		}
		imports := FromRequirements(reqs)
		return graph.Deps{
			Direct:     imports,
			Transitive: fromImports(imports),
		}, nil
	case "requirements":
		fallthrough
	default:
		reqs, err := pip.FromFile(a.requirementsFile(a.Module))
		if err != nil {
			return graph.Deps{}, err
		}
		imports := FromRequirements(reqs)
		return graph.Deps{
			Direct:     imports,
			Transitive: fromImports(imports),
		}, nil
	}
}

func (a *Analyzer) requirementsFile(m module.Module) string {
	reqFilename := filepath.Join(m.Dir, "requirements.txt")
	if a.Options.RequirementsPath != "" {
		reqFilename = a.Options.RequirementsPath
	}
	log.Logger.Debugf("%#v", reqFilename)
	return reqFilename
}

func fromImports(imports []pkg.Import) map[pkg.ID]pkg.Package {
	deps := make(map[pkg.ID]pkg.Package)
	for _, i := range imports {
		deps[i.Resolved] = pkg.Package{
			ID: i.Resolved,
		}
	}
	return deps
}
