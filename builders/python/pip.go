package python

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/builders/builderutil"
	"github.com/fossas/fossa-cli/builders/python/bindata"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

// PipBuilder implements Builder for Pip.
// These properties are public for the sake of serialization.
type PipBuilder struct {
	PythonCmd     string
	PythonVersion string

	PipCmd     string
	PipVersion string

	Virtualenv string
}

// Initialize collects metadata on Python and Pip binaries
func (builder *PipBuilder) Initialize() error {
	log.Logger.Debug("Initializing Pip builder...")

	// Set Python context variables
	pythonCmd, pythonVersion, err := exec.Which("--version", os.Getenv("PYTHON_BINARY"), "python", "python3", "python27", "python2")
	if err != nil {
		log.Logger.Warningf("Could not find Python binary (try setting $PYTHON_BINARY): %s", err.Error())
	}
	builder.PythonCmd = pythonCmd
	builder.PythonVersion = pythonVersion

	// Set Pip context variables
	pipCmd, pipVersion, pipErr := exec.Which("--version", os.Getenv("PIP_BINARY"), "pip")
	builder.PipCmd = pipCmd
	builder.PipVersion = pipVersion
	if pipErr != nil {
		log.Logger.Warningf("No supported Python build tools detected (try setting $PIP_BINARY): %#v", pipErr)
	}

	// Check virtualenv
	builder.Virtualenv = os.Getenv("VIRTUAL_ENV")

	log.Logger.Debugf("Initialized Pip builder: %#v", builder)
	return nil
}

// Build runs `pip install -r requirements.txt`
func (builder *PipBuilder) Build(m module.Module, force bool) error {
	log.Logger.Debugf("Running Pip build: %#v %#v", m, force)

	_, _, err := exec.Run(exec.Cmd{
		Name: builder.PipCmd,
		Argv: []string{"install", "-r", "requirements"},
		Dir:  m.Dir,
	})
	if err != nil {
		return fmt.Errorf("could not run Pip build: %s", err.Error())
	}

	log.Logger.Debug("Done running Pip build.")
	return nil
}

type pipDepTreeDep struct {
	PackageName      string `json:"package_name"`
	InstalledVersion string `json:"installed_version"`
	Dependencies     []pipDepTreeDep
}

func flattenPipDepTree(pkg pipDepTreeDep, from module.ImportPath) []builderutil.Imported {
	var imports []builderutil.Imported
	locator := module.Locator{
		Fetcher:  "pip",
		Project:  pkg.PackageName,
		Revision: pkg.InstalledVersion,
	}
	for _, dep := range pkg.Dependencies {
		imports = append(imports, flattenPipDepTree(dep, append(from, locator))...)
	}
	imports = append(imports, builderutil.Imported{
		Locator: locator,
		From:    append(module.ImportPath{}, from...),
	})
	return imports
}

// Analyze runs `pipdeptree.py` in the current environment
func (builder *PipBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Pip analysis: %#v %#v", m, allowUnresolved)

	// Write helper to disk.
	pipdeptreeSrc, err := bindata.Asset("builders/python/bindata/pipdeptree.py")
	if err != nil {
		return nil, errors.Wrap(err, "could not read `pipdeptree` helper")
	}

	pipdeptreeFile, err := ioutil.TempFile("", "fossa-cli-pipdeptree")
	if err != nil {
		return nil, errors.Wrap(err, "could not create temp file to write `pipdeptree` helper")
	}
	defer pipdeptreeFile.Close()
	defer os.Remove(pipdeptreeFile.Name())

	n, err := pipdeptreeFile.Write(pipdeptreeSrc)
	if len(pipdeptreeSrc) != n || err != nil {
		return nil, errors.Wrap(err, "could not write `pipdeptree` helper")
	}
	err = pipdeptreeFile.Sync()
	if err != nil {
		return nil, errors.Wrap(err, "could not flush `pipdeptree` helper to storage")
	}

	// Run helper.
	out, _, err := exec.Run(exec.Cmd{
		Name: builder.PythonCmd,
		Argv: []string{pipdeptreeFile.Name(), "--local-only", "--json-tree"},
		Dir:  m.Dir,
	})
	if err != nil {
		return nil, errors.Wrap(err, "could not run `pipdeptree`")
	}

	// Parse output.
	var pipdeptreeOutput []pipDepTreeDep
	err = json.Unmarshal([]byte(out), &pipdeptreeOutput)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse `pipdeptree` output")
	}

	// Flatten tree.
	var imports []builderutil.Imported
	for _, dep := range pipdeptreeOutput {
		imports = append(imports, flattenPipDepTree(dep, module.ImportPath{
			module.Locator{
				Fetcher:  "root",
				Project:  "root",
				Revision: "",
			},
		})...)
	}
	deps := builderutil.ComputeImportPaths(imports)

	log.Logger.Debugf("Done running Pip analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `requirements.txt`
func (builder *PipBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Logger.Debugf("Checking Pip build: %#v %#v", m, allowUnresolved)

	// TODO: support `pip freeze` output and other alternative methods?
	isBuilt, err := files.Exists(m.Dir, "requirements.txt")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Pip build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *PipBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for PipBuilder")
}

// DiscoverModules builds ModuleConfigs for any `requirements.txt` files
func (builder *PipBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var moduleConfigs []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		if !info.IsDir() && info.Name() == "requirements.txt" {
			moduleName := filepath.Base(filepath.Dir(path))

			log.Logger.Debugf("Found Python package: %s (%s)", path, moduleName)
			path, _ = filepath.Rel(dir, path)
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: moduleName,
				Path: path,
				Type: string(module.Pip),
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Python package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}
