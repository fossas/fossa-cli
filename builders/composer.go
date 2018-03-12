package builders

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var composerLogger = logging.MustGetLogger("composer")

// ComposerPackage implements Dependency for Composer
type ComposerPackage struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns comp for ComposerPackage
func (m ComposerPackage) Fetcher() string {
	return "comp"
}

// Package returns the package spec for ComposerPackage
func (m ComposerPackage) Package() string {
	return m.Name
}

// Revision returns the version spec for ComposerPackage
func (m ComposerPackage) Revision() string {
	return m.Version
}

// ComposerBuilder implements Builder for Composer (composer.json) builds
type ComposerBuilder struct {
	PHPCmd     string
	PHPVersion string

	ComposerCmd     string
	ComposerVersion string
}

// Initialize collects metadata on PHP and Composer binaries
func (builder *ComposerBuilder) Initialize() error {
	composerLogger.Debug("Initializing Composer builder...")

	// Set PHP context variables
	phpCmd, phpVersion, err := which("-v", os.Getenv("PHP_BINARY"), "php")
	if err != nil {
		return fmt.Errorf("could not find PHP binary (try setting $PHP_BINARY): %s", err.Error())
	}
	builder.PHPCmd = phpCmd
	builder.PHPVersion = phpVersion

	// Set Composer context variables
	composerCmd, composerVersion, err := which("-V", os.Getenv("COMPOSER_BINARY"), "composer")
	if err != nil {
		return fmt.Errorf("could not find Composer binary (try setting $COMPOSER_BINARY): %s", err.Error())
	}
	builder.ComposerCmd = composerCmd
	builder.ComposerVersion = composerVersion

	composerLogger.Debugf("Done initializing Composer builder: %#v", builder)
	return nil
}

// Build runs `composer install --prefer-dist --no-dev` and cleans with `rm -rf vendor`
func (builder *ComposerBuilder) Build(m module.Module, force bool) error {
	composerLogger.Debug("Running Composer build: %#v %#v", m, force)

	if force {
		_, _, err := runLogged(composerLogger, m.Dir, "rm", "-rf", "vendor")
		if err != nil {
			return fmt.Errorf("could not remove Composer cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(composerLogger, m.Dir, builder.ComposerCmd, "install", "--prefer-dist", "--no-dev")
	if err != nil {
		return fmt.Errorf("could not run Composer build: %s", err.Error())
	}

	composerLogger.Debug("Done running Composer build.")
	return nil
}

// Analyze parses the output of `composer show -f json --no-ansi`
func (builder *ComposerBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	composerLogger.Debug("Running Composer analysis: %#v %#v", m, allowUnresolved)

	// Run `composer show -f json --no-ansi`
	output, _, err := runLogged(composerLogger, m.Dir, builder.ComposerCmd, "show", "-f", "json", "--no-ansi")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from Composer: %s", err.Error())
	}

	// Parse output as JSON
	composerJSON := map[string][]ComposerPackage{}
	err = json.Unmarshal([]byte(output), &composerJSON)
	if err != nil {
		return nil, fmt.Errorf("could not parse dependency list as JSON: %#v %#v", err.Error(), output)
	}

	// Get dependencies from "installed" key of Composer output
	var deps []module.Dependency
	for _, d := range composerJSON["installed"] {
		deps = append(deps, d)
	}

	composerLogger.Debugf("Done running Composer analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether `composer show --no-ansi` produces output
func (builder *ComposerBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	composerLogger.Debugf("Checking Composer build: %#v %#v", m, allowUnresolved)

	// Run `composer show --no-ansi`
	output, _, err := runLogged(composerLogger, m.Dir, builder.ComposerCmd, "show", "--no-ansi")
	if err != nil {
		return false, fmt.Errorf("could not get dependency list from Composer: %s", err.Error())
	}

	// Check that the output is non-empty
	isBuilt := len(strings.TrimSpace(string(output))) > 0

	composerLogger.Debugf("Done checking Composer build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *ComposerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for ComposerBuilder")
}

// DiscoverModules finds composer.json modules not a /vendor/ folder
func (builder *ComposerBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var moduleConfigs []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			composerLogger.Debugf("failed to access path %s: %s\n", path, err.Error())
			return err
		}

		// Skip the /vendor/ folder
		if info.IsDir() && info.Name() == "vendor" {
			composerLogger.Debugf("skipping `vendor` directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "composer.json" {
			moduleName := filepath.Base(filepath.Dir(path))

			// Parse from composer.json and set moduleName if successful
			var composerPackage ComposerPackage
			if err := parseLogged(composerLogger, path, &composerPackage); err == nil {
				moduleName = composerPackage.Name
			}

			nodejsLogger.Debugf("found Compower package: %s (%s)", path, moduleName)
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: moduleName,
				Path: path,
				Type: string(module.Composer),
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find Composer package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}
