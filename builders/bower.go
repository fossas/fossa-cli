package builders

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var bowerLogger = logging.MustGetLogger("bower")

// BowerComponent implements Dependency for BowerBuilder
type BowerComponent struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns bower for BowerComponent
func (m BowerComponent) Fetcher() string {
	return "bower" // TODO: support `git` and etc...
}

// Package returns the package name for BowerComponent
func (m BowerComponent) Package() string {
	return m.Name
}

// Revision returns the version for BowerComponent
func (m BowerComponent) Revision() string {
	return m.Version
}

// BowerBuilder implements Builder for Bower
type BowerBuilder struct {
	NodeCmd     string
	NodeVersion string

	BowerCmd     string
	BowerVersion string
}

// Initialize collects metadata on Node and Bower binaries
func (builder *BowerBuilder) Initialize() error {
	bowerLogger.Debug("Initializing Bower builder...")

	// Set Node context variables
	nodeCmd, nodeVersion, err := which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
	if err != nil {
		return fmt.Errorf("could not find Node binary (try setting $NODE_BINARY): %s", err.Error())
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set Bower context variables
	bowerCmd, bowerVersion, err := which("-v", os.Getenv("BOWER_BINARY"), "bower")
	if err != nil {
		return fmt.Errorf("could not find Bower binary (try setting $BOWER_BINARY): %s", err.Error())
	}
	builder.BowerCmd = bowerCmd
	builder.BowerVersion = bowerVersion

	bowerLogger.Debugf("Done initializing Bower builder: %#v", builder)
	return nil
}

// Build runs `bower install --production` and cleans with `rm -rf bower_components`
func (builder *BowerBuilder) Build(m module.Module, force bool) error {
	bowerLogger.Debugf("Running Bower build: %#v", m, force)

	if force {
		_, _, err := runLogged(bowerLogger, m.Dir, "rm", "-rf", "bower_components")
		if err != nil {
			return fmt.Errorf("could not remove Bower cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(bowerLogger, m.Dir, builder.BowerCmd, "install", "--production")
	if err != nil {
		return fmt.Errorf("could not run Bower build: %s", err.Error())
	}

	bowerLogger.Debug("Done running Bower build.")
	return nil
}

// Analyze reads dependency manifests at `$PROJECT/**/bower_components/*/.bower.json`
func (builder *BowerBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	bowerLogger.Debugf("Running Bower analysis: %#v %#v", m, allowUnresolved)

	// Find manifests.
	bowerComponents, err := doublestar.Glob(filepath.Join(m.Dir, "**", "bower_components", "*", ".bower.json"))
	if err != nil {
		return nil, fmt.Errorf("could not find Bower dependency manifests: %s", err.Error())
	}
	bowerLogger.Debugf("Found %#v modules from globstar: %#v", len(bowerComponents), bowerComponents)

	// Read manifests.
	var wg sync.WaitGroup
	deps := make([]module.Dependency, len(bowerComponents))
	wg.Add(len(bowerComponents))
	for i := 0; i < len(bowerComponents); i++ {
		go func(modulePath string, index int, wg *sync.WaitGroup) {
			defer wg.Done()

			var bowerComponent BowerComponent
			parseLogged(bowerLogger, modulePath, &bowerComponent)

			// Write directly to a reserved index for thread safety
			deps[index] = bowerComponent
		}(bowerComponents[i], i, &wg)
	}
	wg.Wait()

	bowerLogger.Debugf("Done running Bower analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `$PROJECT/bower_components`
func (builder *BowerBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	bowerLogger.Debug("Checking Bower build: %#v %#v", m, allowUnresolved)

	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	isBuilt, err := hasFile(m.Dir, "bower_components")
	if err != nil {
		return false, fmt.Errorf("could not find Bower dependencies folder: %s", err.Error())
	}

	bowerLogger.Debugf("Done checking Bower build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *BowerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for BowerBuilder")
}

// DiscoverModules finds any bower.json modules not in node_modules or bower_components folders
func (builder *BowerBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var moduleConfigs []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			bowerLogger.Debugf("Failed to access path %s: %s", path, err.Error())
			return err
		}
		// Skip **/node_modules and **/bower_components directories
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			bowerLogger.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "bower.json" {
			moduleName := filepath.Base(filepath.Dir(path))

			// Parse from bower.json and set moduleName if successful
			var bowerComponent BowerComponent
			if err := parseLogged(bowerLogger, path, &bowerComponent); err == nil {
				moduleName = bowerComponent.Name
			}

			bowerLogger.Debugf("Found Bower package: %s (%s)", path, moduleName)
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: moduleName,
				Path: path,
				Type: string(module.Bower),
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find bower package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}
