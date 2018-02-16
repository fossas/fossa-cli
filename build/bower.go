package build

import (
	"encoding/json"
	"errors"
	"io/ioutil"
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

// Initialize collects Node and Bower binaries
func (builder *BowerBuilder) Initialize() error {
	bowerLogger.Debug("Initializing Bower builder...")

	// Set Node context variables
	nodeCmd, nodeVersion, err := which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
	if err != nil {
		return errors.New("could not find Node binary (try setting $NODE_BINARY)")
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set Bower context variables
	bowerCmd, bowerVersion, err := which("-v", os.Getenv("BOWER_BINARY"), "bower")
	if err != nil {
		return errors.New("could not find Bower binary (try setting $BOWER_BINARY)")
	}
	builder.BowerCmd = bowerCmd
	builder.BowerVersion = bowerVersion

	bowerLogger.Debugf("Done initializing Bower builder: %#v", builder)
	return nil
}

// Build runs `bower install --production` and cleans with `rm -rf bower_components`
func (builder *BowerBuilder) Build(m module.Module, force bool) error {
	bowerLogger.Debug("Running Bower build...")

	if force {
		bowerLogger.Debug("`force` flag is set: running `rm -rf bower_components`...")
		_, _, err := runLogged(bowerLogger, m.Dir, "rm", "-rf", "bower_components")
		if err != nil {
			return err
		}
	}

	_, _, err := runLogged(bowerLogger, m.Dir, builder.BowerCmd, "install", "--production")
	if err != nil {
		return err
	}

	bowerLogger.Debug("Done running Bower build.")
	return nil
}

// Analyze reads package manifests at `$PROJECT/**/bower_components/*/.bower.json`
func (builder *BowerBuilder) Analyze(m module.Module, _ bool) ([]module.Dependency, error) {
	bowerLogger.Debug("Running Bower analysis...")

	// Find modules.
	bowerComponents, err := doublestar.Glob(filepath.Join(m.Dir, "**", "bower_components", "*", ".bower.json"))
	if err != nil {
		return nil, err
	}
	bowerLogger.Debugf("Found %#v modules from globstar: %#v", len(bowerComponents), bowerComponents)

	// Read manifests.
	var wg sync.WaitGroup
	deps := make([]module.Dependency, len(bowerComponents))
	wg.Add(len(bowerComponents))
	for i := 0; i < len(bowerComponents); i++ {
		go func(modulePath string, index int, wg *sync.WaitGroup) {
			defer wg.Done()

			dependencyManifest, err := ioutil.ReadFile(modulePath)
			if err != nil {
				bowerLogger.Warningf("Error parsing .bower.json: %#v", modulePath)
				return
			}

			// Write directly to a reserved index for thread safety
			var bowerComponent BowerComponent
			json.Unmarshal(dependencyManifest, &bowerComponent)
			deps[index] = bowerComponent
		}(bowerComponents[i], i, &wg)
	}
	wg.Wait()

	bowerLogger.Debugf("Done running Bower analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `$PROJECT/bower_components`
func (builder *BowerBuilder) IsBuilt(m module.Module, _ bool) (bool, error) {
	bowerLogger.Debug("Checking Bower build...")

	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	ok, err := hasFile(m.Dir, "bower_components")
	if err != nil {
		return false, err
	}
	return ok, nil
}

// IsModule is not implemented
func (builder *BowerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for BowerBuilder")
}

// InferModule is not implemented
func (builder *BowerBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for BowerBuilder")
}
