package builders

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

type BowerConfiguration struct {
	Cwd       string `json:"cwd"`
	Directory string `json:"directory"`
	Registry  string `json:"registry"`
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
	log.Logger.Debug("Initializing Bower builder...")

	// Set Node context variables
	nodeCmd, nodeVersion, err := which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
	if err != nil {
		log.Logger.Warningf("Could not find Node binary (try setting $NODE_BINARY): %s", err.Error())
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set Bower context variables
	bowerCmd, bowerVersion, err := which("-v", os.Getenv("BOWER_BINARY"), "bower")
	if err != nil {
		return errors.Wrap(err, "could not find Bower binary (try setting $BOWER_BINARY)")
	}
	builder.BowerCmd = bowerCmd
	builder.BowerVersion = bowerVersion

	log.Logger.Debugf("Done initializing Bower builder: %#v", builder)
	return nil
}

// Build runs `bower install --production` and cleans with `rm -rf bower_components`
func (builder *BowerBuilder) Build(m module.Module, force bool) error {
	log.Logger.Debugf("Running Bower build: %#v", m, force)

	if force {
		_, _, err := runLogged(m.Dir, "rm", "-rf", "bower_components")
		if err != nil {
			return fmt.Errorf("could not remove Bower cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(m.Dir, builder.BowerCmd, "install", "--production")
	if err != nil {
		return fmt.Errorf("could not run Bower build: %s", err.Error())
	}

	log.Logger.Debug("Done running Bower build.")
	return nil
}

type bowerListManifest struct {
	PkgMeta struct {
		Name    string
		Version string
	}
	Dependencies map[string]bowerListManifest
}

type bowerJSONManifest struct {
	Name    string
	Version string
}

func normalizeBowerComponents(parent module.ImportPath, c bowerListManifest) []Imported {
	var deps []Imported
	for _, dep := range c.Dependencies {
		deps = append(
			deps,
			normalizeBowerComponents(
				append(parent, module.Locator{
					Fetcher:  "bower",
					Project:  c.PkgMeta.Name,
					Revision: c.PkgMeta.Version,
				}),
				dep,
			)...)
	}

	return append(deps, Imported{
		Locator: module.Locator{
			Fetcher:  "bower",
			Project:  c.PkgMeta.Name,
			Revision: c.PkgMeta.Version,
		},
		From: append(module.ImportPath{}, parent...),
	})
}

// Analyze reads the output of `bower ls --json`
// TODO: fall back to old method of reading `bower_components/*/.bower.json`s?
func (builder *BowerBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Bower analysis: %#v %#v", m, allowUnresolved)

	stdout, _, err := runLogged(m.Dir, "bower", "ls", "--json")
	if err != nil {
		return nil, errors.Wrap(err, "could not run `bower ls --json`")
	}

	var output bowerListManifest
	err = json.Unmarshal([]byte(stdout), &output)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse `bower ls --json` output")
	}

	var depList []Imported
	for _, dep := range output.Dependencies {
		depList = append(
			depList,
			normalizeBowerComponents(
				[]module.Locator{
					module.Locator{
						Fetcher:  "root",
						Project:  "root", // TODO: This should be the project name, we'll need to pass that in via the module
						Revision: "root",
					},
				},
				dep,
			)...,
		)
	}
	deps := computeImportPaths(depList)

	log.Logger.Debugf("Done running Bower analysis: %#v", deps)
	return deps, nil
}

// resolveBowerComponentsDirectory resolves a component dir from a `.bowerrc` file, falling back to `bower_components`
func resolveBowerComponentsDirectory(dir string) string {
	bowerConfigPath := filepath.Join(dir, ".bowerrc")
	bowerComponentsPath := filepath.Join(dir, "bower_components")

	if bowerConfigExists, _ := hasFile(bowerConfigPath); bowerConfigExists {
		var bowerConfiguration BowerConfiguration
		parseLogged(bowerConfigPath, &bowerConfiguration)

		if bowerConfiguration.Directory != "" {
			bowerComponentsPath = bowerConfiguration.Directory
		}
	}

	return bowerComponentsPath
}

// IsBuilt checks for the existence of `$PROJECT/bower_components`
func (builder *BowerBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Logger.Debug("Checking Bower build: %#v %#v", m, allowUnresolved)

	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	isBuilt, err := hasFile(resolveBowerComponentsDirectory(m.Dir))
	if err != nil {
		return false, fmt.Errorf("could not find Bower dependencies folder: %s", err.Error())
	}

	log.Logger.Debugf("Done checking Bower build: %#v", isBuilt)
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
			log.Logger.Debugf("Failed to access path %s: %s", path, err.Error())
			return err
		}
		// Skip **/node_modules and **/bower_components directories
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Logger.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "bower.json" {
			moduleName := filepath.Base(filepath.Dir(path))

			// Parse from bower.json and set moduleName if successful
			var manifest bowerJSONManifest
			if err := parseLogged(path, &manifest); err == nil {
				moduleName = manifest.Name
			}

			log.Logger.Debugf("Found Bower package: %s (%s)", path, moduleName)
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
