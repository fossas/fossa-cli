package build

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

var nodejsLogger = logging.MustGetLogger("nodejs")

// NodeModule implements Dependency for NodeJSBuilder.
type NodeModule struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns npm for NodeModule. TODO: Support git and other
// dependency sources.
func (m NodeModule) Fetcher() string {
	return "npm" // TODO: support git and etc...
}

// Package returns the package name for NodeModule
func (m NodeModule) Package() string {
	return m.Name
}

// Revision returns the version for NodeModule
func (m NodeModule) Revision() string {
	return m.Version
}

// NodeJSBuilder implements Builder for Nodejs.
// These properties are public for the sake of serialization.
type NodeJSBuilder struct {
	NodeCmd     string
	NodeVersion string

	NPMCmd     string
	NPMVersion string

	YarnCmd     string
	YarnVersion string
}

// Initialize collects environment data for Nodejs builds
func (builder *NodeJSBuilder) Initialize() error {
	nodejsLogger.Debugf("Initializing Nodejs builder...")

	// Set NodeJS context variables
	nodeCmd, nodeVersion, err := which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
	if err != nil {
		return fmt.Errorf("could not find Node binary (try setting $NODE_BINARY): %s", err.Error())
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set NPM context variables
	npmCmd, npmVersion, npmErr := which("-v", os.Getenv("NPM_BINARY"), "npm")
	builder.NPMCmd = npmCmd
	builder.NPMVersion = npmVersion

	// Set Yarn context variables
	yarnCmd, yarnVersion, yarnErr := which("-v", os.Getenv("YARN_BINARY"), "yarn")
	builder.YarnCmd = yarnCmd
	builder.YarnVersion = yarnVersion

	if npmErr != nil && yarnErr != nil {
		return fmt.Errorf("no supported Nodejs build tools detected (try setting $NPM_BINARY or $YARN_BINARY): %#v %#v", npmErr, yarnErr)
	}

	nodejsLogger.Debugf("Initialized Nodejs builder: %#v", builder)
	return nil
}

// Build runs either `yarn install --production --frozen-lockfile` or `npm install --production` and cleans with `rm -rf node_modules`
func (builder *NodeJSBuilder) Build(m module.Module, force bool) error {
	nodejsLogger.Debugf("Running Nodejs build: %#v %#v", m, force)

	if force {
		_, _, err := runLogged(nodejsLogger, m.Dir, "rm", "-rf", "node_modules")
		if err != nil {
			return fmt.Errorf("could not remove Nodejs cache: %s", err.Error())
		}
	}

	// Prefer Yarn where possible
	if ok, err := hasFile(m.Dir, "yarn.lock"); err == nil && ok {
		_, _, err := runLogged(nodejsLogger, m.Dir, builder.YarnCmd, "install", "--production", "--frozen-lockfile")
		if err != nil {
			return fmt.Errorf("could not run Yarn build: %s", err.Error())
		}
	} else {
		_, _, err := runLogged(nodejsLogger, m.Dir, builder.NPMCmd, "install", "--production")
		if err != nil {
			return fmt.Errorf("could not run NPM build: %s", err.Error())
		}
	}

	nodejsLogger.Debug("Done running Nodejs build.")
	return nil
}

// Analyze reads dependency manifests at `$PROJECT/**/node_modules/*/package.json`
func (builder *NodeJSBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	nodejsLogger.Debugf("Running Nodejs analysis: %#v %#v", m, allowUnresolved)

	// Find manifests.
	nodeModules, err := doublestar.Glob(filepath.Join(m.Dir, "**", "node_modules", "*", "package.json"))
	if err != nil {
		return nil, fmt.Errorf("could not find Nodejs dependency manifests: %s", err.Error())
	}
	nodejsLogger.Debugf("Found %#v modules from globstar: %#v", len(nodeModules), nodeModules)

	// Read manifests.
	var wg sync.WaitGroup
	deps := make([]module.Dependency, len(nodeModules))
	wg.Add(len(nodeModules))
	for i := 0; i < len(nodeModules); i++ {
		go func(modulePath string, index int, wg *sync.WaitGroup) {
			defer wg.Done()

			var nodeModule NodeModule
			parseLogged(nodejsLogger, modulePath, &nodeModule)

			// Write directly to a reserved index for thread safety
			deps[index] = nodeModule
		}(nodeModules[i], i, &wg)
	}
	wg.Wait()

	nodejsLogger.Debugf("Done running Nodejs analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `$PROJECT/node_modules`
func (builder *NodeJSBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	nodejsLogger.Debugf("Checking Nodejs build: %#v %#v", m, allowUnresolved)

	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	isBuilt, err := hasFile(m.Dir, "node_modules")
	if err != nil {
		return false, fmt.Errorf("could not find Nodejs dependencies folder: %s", err.Error())
	}

	nodejsLogger.Debugf("Done checking Nodejs build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *NodeJSBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for NodeJSBuilder")
}

// InferModule is not implemented
func (builder *NodeJSBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for NodeJSBuilder")
}
