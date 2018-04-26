package builders

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/module"
	logging "github.com/op/go-logging"
)

var pipLogger = logging.MustGetLogger("pip")

// PipBuilder implements Builder for Pip.
// These properties are public for the sake of serialization.
type PipBuilder struct {
	PythonCmd     string
	PythonVersion string

	PipCmd     string
	PipVersion string
}

// Initialize collects metadata on Python and Pip binaries
func (builder *PipBuilder) Initialize() error {
	pipLogger.Debug("Initializing Pip builder...")

	// Set Python context variables
	pythonCmd, pythonVersion, err := which("--version", os.Getenv("PYTHON_BINARY"), "python", "python3", "python27", "python2")
	if err != nil {
		pipLogger.Warningf("Could not find Python binary (try setting $PYTHON_BINARY): %s", err.Error())
	}
	builder.PythonCmd = pythonCmd
	builder.PythonVersion = pythonVersion

	// Set Pip context variables
	pipCmd, pipVersion, pipErr := which("--version", os.Getenv("PIP_BINARY"), "pip")
	builder.PipCmd = pipCmd
	builder.PipVersion = pipVersion
	if pipErr != nil {
		pipLogger.Warningf("No supported Python build tools detected (try setting $PIP_BINARY): %#v", pipErr)
	}

	pipLogger.Debugf("Initialized Pip builder: %#v", builder)
	return nil
}

// Build runs `pip install -r requirements.txt`
func (builder *PipBuilder) Build(m module.Module, force bool) error {
	pipLogger.Debugf("Running Pip build: %#v %#v", m, force)

	_, _, err := runLogged(pipLogger, m.Dir, builder.PipCmd, "install", "-r", "requirements.txt")
	if err != nil {
		return fmt.Errorf("could not run Pip build: %s", err.Error())
	}

	pipLogger.Debug("Done running Pip build.")
	return nil
}

// Analyze reads `requirements.txt`
func (builder *PipBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	pipLogger.Debugf("Running Pip analysis: %#v %#v", m, allowUnresolved)

	lockfile := filepath.Join(m.Dir, "requirements.txt")
	lockfileContents, err := ioutil.ReadFile(lockfile)
	if err != nil {
		goLogger.Debugf("Error reading %s: %s", lockfile, err.Error())
		return nil, fmt.Errorf("could not read %s: %s", lockfile, err.Error())
	}

	lines := strings.Split(string(lockfileContents), "\n")
	lockfileVersions := make(map[string]string)
	for _, line := range lines {
		trimmedLine := strings.TrimSpace(line)
		if len(trimmedLine) > 0 && trimmedLine[0] != '#' {
			// TODO: support other kinds of constraints
			sections := strings.Split(trimmedLine, "==")
			lockfileVersions[sections[0]] = sections[1]
		}
	}

	var deps []module.Dependency
	for pkg, version := range lockfileVersions {
		deps = append(deps, module.Dependency{
			Locator: module.Locator{
				Fetcher:  "pip",
				Project:  pkg,
				Revision: version,
			},
			Via: nil,
		})
	}

	pipLogger.Debugf("Done running Pip analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `requirements.txt`
func (builder *PipBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	pipLogger.Debugf("Checking Pip build: %#v %#v", m, allowUnresolved)

	// TODO: support `pip freeze` output and other alternative methods?
	isBuilt, err := hasFile(m.Dir, "requirements.txt")
	if err != nil {
		return false, fmt.Errorf("could not find `requirements.txt`: %s", err.Error())
	}

	pipLogger.Debugf("Done checking Pip build: %#v", isBuilt)
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
			pipLogger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		if !info.IsDir() && info.Name() == "requirements.txt" {
			moduleName := filepath.Base(filepath.Dir(path))

			pipLogger.Debugf("Found Python package: %s (%s)", path, moduleName)
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
