package python

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/pip"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	PythonCmd     string
	PythonVersion string

	Options Options
}

type Options struct {
	Strategy         string `mapstructure:"strategy"`
	RequirementsPath string `mapstructure:"requirements"`
	VirtualEnv       string `mapstructure:"venv"`
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debug("%#v", opts)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	// Construct analyzer.
	cmd, version, err := exec.Which("--version", os.Getenv("FOSSA_PYTHON_CMD"), "python", "python3", "python2.7")
	if err != nil {
		return nil, err
	}
	return &Analyzer{
		PythonCmd:     cmd,
		PythonVersion: version,
		Options:       options,
	}, nil
}

// Discover constructs modules in all directories with a `requirements.txt` or
// `setup.py`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
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
func (a *Analyzer) Clean(m module.Module) error {
	log.Logger.Warningf("Clean is not implemented for Python")
	return nil
}

func (a *Analyzer) Build(m module.Module) error {
	p := pip.Pip{
		PythonCmd: a.PythonCmd,
	}
	return p.Install(a.reqFile(m))
}

func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	return true, nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	p := pip.Pip{
		PythonCmd: a.PythonCmd,
	}
	switch a.Options.Strategy {
	case "deptree":
		tree, err := p.DepTree()
		if err != nil {
			return m, err
		}
		imports, graph := FromTree(tree)
		m.Imports = imports
		m.Deps = graph
		return m, nil
	case "pip":
		reqs, err := p.List()
		if err != nil {
			return m, err
		}
		m.Imports = FromRequirements(reqs)
		return m, nil
	case "requirements":
		fallthrough
	default:
		reqs, err := pip.FromFile(a.reqFile(m))
		if err != nil {
			return m, err
		}
		m.Imports = FromRequirements(reqs)
		return m, nil
	}
}

func (a *Analyzer) reqFile(m module.Module) string {
	reqFilename := filepath.Join(m.Dir, "requirements.txt")
	if a.Options.RequirementsPath != "" {
		reqFilename = a.Options.RequirementsPath
	}
	log.Logger.Debugf("%#v", reqFilename)
	return reqFilename
}
