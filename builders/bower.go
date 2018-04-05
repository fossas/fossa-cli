package builders

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	logging "github.com/op/go-logging"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/module"
)

var bowerLogger = logging.MustGetLogger("bower")

// BowerComponent implements Dependency for BowerBuilder
type BowerComponent struct {
	Name    string
	Version string
	Deps    []BowerComponent
	Via     [][]string
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

// Dependencies returns the deeper dependencies of any dependency.
func (m BowerComponent) Dependencies() []module.Dependency {
	var deps []module.Dependency
	for _, d := range m.Deps {
		deps = append(deps, d)
	}
	return deps
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
		bowerLogger.Warningf("Could not find Node binary (try setting $NODE_BINARY): %s", err.Error())
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set Bower context variables
	bowerCmd, bowerVersion, err := which("-v", os.Getenv("BOWER_BINARY"), "bower")
	if err != nil {
		bowerLogger.Fatalf("Could not find Bower binary (try setting $BOWER_BINARY): %s", err.Error())
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

type bowerListOutput struct {
	PkgMeta struct {
		Name    string
		Version string
	}
	Dependencies map[string]bowerListOutput
}

func normalizeBowerComponents(parent []string, component bowerListOutput) []BowerComponent {
	// Initializing this way makes `deps` marshal as `[]` when empty instead of `null`
	deps := make([]BowerComponent, 0)
	for _, dep := range component.Dependencies {
		deps = append(deps, normalizeBowerComponents(append(parent, component.PkgMeta.Name+"@"+component.PkgMeta.Version), dep)...)
	}
	return append(deps, BowerComponent{
		Name:    component.PkgMeta.Name,
		Version: component.PkgMeta.Version,
		Deps:    deps,
		Via:     [][]string{parent},
	})
}

// Analyze reads the output of `bower ls --json`
func (builder *BowerBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	bowerLogger.Debugf("Running Bower analysis: %#v %#v", m, allowUnresolved)

	stdout, _, err := runLogged(bowerLogger, m.Dir, "bower", "ls", "--json")
	if err != nil {
		return nil, errors.Wrap(err, "could not run `bower ls --json`")
	}

	var output bowerListOutput
	err = json.Unmarshal([]byte(stdout), &output)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse `bower ls --json` output")
	}

	// Compute origin paths
	var deps []BowerComponent
	for _, dep := range output.Dependencies {
		deps = append(deps, normalizeBowerComponents([]string{"."}, dep)...)
	}
	paths := make(map[[2]string]map[string]bool)
	for _, d := range deps {
		i := [2]string{d.Name, d.Version}
		_, ok := paths[i]
		if !ok {
			paths[i] = make(map[string]bool)
		}
		paths[i][strings.Join(d.Via[0], " ")] = true
	}

	var moduleDeps []module.Dependency
	for d, ps := range paths {
		var modulePaths [][]string
		for p := range ps {
			modulePaths = append(modulePaths, strings.Split(p, " "))
		}
		moduleDeps = append(moduleDeps, BowerComponent{
			Name:    d[0],
			Version: d[1],
			Via:     modulePaths,
		})
	}

	bowerLogger.Debugf("Done running Bower analysis: %#v", deps)
	return moduleDeps, nil
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
