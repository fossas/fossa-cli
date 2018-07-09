package nodejs

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/builders/builderutil"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

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

// Initialize collects metadata on Node, NPM, and Yarn binaries
func (builder *NodeJSBuilder) Initialize() error {
	log.Logger.Debug("Initializing Nodejs builder...")

	// Set NodeJS context variables
	nodeCmd, nodeVersion, err := exec.Which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
	if err != nil {
		log.Logger.Warningf("Could not find Node binary (try setting $NODE_BINARY): %s", err.Error())
	}
	builder.NodeCmd = nodeCmd
	builder.NodeVersion = nodeVersion

	// Set NPM context variables
	npmCmd, npmVersion, npmErr := exec.Which("-v", os.Getenv("NPM_BINARY"), "npm")
	builder.NPMCmd = npmCmd
	builder.NPMVersion = npmVersion

	// Set Yarn context variables
	yarnCmd, yarnVersion, yarnErr := exec.Which("-v", os.Getenv("YARN_BINARY"), "yarn")
	builder.YarnCmd = yarnCmd
	builder.YarnVersion = yarnVersion

	if npmErr != nil && yarnErr != nil {
		log.Logger.Warningf("No supported Nodejs build tools detected (try setting $NPM_BINARY or $YARN_BINARY): %#v %#v", npmErr, yarnErr)
	}

	log.Logger.Debugf("Initialized Nodejs builder: %#v", builder)
	return nil
}

// Build runs either `yarn install --production --frozen-lockfile` or `npm install --production` and cleans with `rm -rf node_modules`
func (builder *NodeJSBuilder) Build(m module.Module, force bool) error {
	log.Logger.Debugf("Running Nodejs build: %#v %#v", m, force)

	if force {
		_, _, err := exec.Run(exec.Cmd{
			Name: "rm",
			Argv: []string{"-rf", "node_modules"},
			Dir:  m.Dir,
		})
		if err != nil {
			return fmt.Errorf("could not remove Nodejs cache: %s", err.Error())
		}
	}

	// Prefer Yarn where possible
	if ok, err := files.Exists(m.Dir, "yarn.lock"); err != nil && ok {
		_, _, err := exec.Run(exec.Cmd{
			Name: builder.YarnCmd,
			Argv: []string{"install", "--production", "--frozen-lockfile"},
			Dir:  m.Dir,
		})
		if err != nil {
			return fmt.Errorf("could not run Yarn build: %s", err.Error())
		}
	} else {
		_, _, err := exec.Run(exec.Cmd{
			Name: builder.NPMCmd,
			Argv: []string{"install", "--production"},
			Dir:  m.Dir,
		})
		if err != nil {
			return fmt.Errorf("could not run NPM build: %s", err.Error())
		}
	}

	log.Logger.Debug("Done running Nodejs build.")
	return nil
}

type NodeManifest struct {
	Name         string
	Version      string
	Dependencies map[string]string
}

type nodeListOutput struct {
	Version      string
	Dependencies map[string]nodeListOutput
}

func flattenNodeJSModulesRecurse(locator module.Locator, pkg nodeListOutput, from module.ImportPath) []builderutil.Imported {
	var imports []builderutil.Imported
	for dep, manifest := range pkg.Dependencies {
		transitive := flattenNodeJSModulesRecurse(module.Locator{
			Fetcher:  "npm",
			Project:  dep,
			Revision: manifest.Version,
		}, manifest, append(from, locator))
		imports = append(imports, transitive...)
	}
	imports = append(imports, builderutil.Imported{
		Locator: locator,
		From:    append(module.ImportPath{}, from...),
	})
	return imports
}

func flattenNodeJSModules(pkg nodeListOutput) []builderutil.Imported {
	root := module.Locator{
		Fetcher:  "root",
		Project:  "root",
		Revision: "",
	}
	imports := flattenNodeJSModulesRecurse(root, pkg, module.ImportPath{})
	// Don't include extraneous "root" dependency
	return imports[:len(imports)-1]
}

// Analyze runs and parses `npm ls --json`.
func (builder *NodeJSBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Nodejs analysis: %#v %#v", m, allowUnresolved)

	// TODO: we must allow this to exit with error if a flag is passed (maybe --allow-npm-err)
	// because sometimes npm will throw errors even after a complete install
	out, stderr, err := exec.Run(exec.Cmd{
		Name: builder.NPMCmd,
		Argv: []string{"ls", "--json"},
		Dir:  m.Dir,
	})
	if err != nil {
		log.Logger.Warningf("NPM had non-zero exit code: %s", stderr)
	}

	var parsed nodeListOutput
	err = json.Unmarshal([]byte(out), &parsed)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse `npm ls --json` output")
	}
	imports := flattenNodeJSModules(parsed)
	deps := builderutil.ComputeImportPaths(imports)

	log.Logger.Debugf("Done running Nodejs analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks for the existence of `$PROJECT/node_modules`
func (builder *NodeJSBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Logger.Debugf("Checking Nodejs build: %#v %#v", m, allowUnresolved)

	if allowUnresolved {
		return true, nil
	}

	// test: there are some package.json with no deps (no node_modules)
	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	var manifest NodeManifest
	err := files.ReadJSON(&manifest, filepath.Join(m.Dir, "package.json"))
	if err != nil {
		return false, err
	}
	if len(manifest.Dependencies) == 0 {
		return true, nil
	}
	isBuilt, err := files.ExistsFolder(m.Dir, "node_modules")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Nodejs build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *NodeJSBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for NodeJSBuilder")
}

// DiscoverModules builds ModuleConfigs for any package.jsons that are not contained in a node_modules dir
func (builder *NodeJSBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var moduleConfigs []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}
		// Skip **/node_modules and **/bower_components
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Logger.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "package.json" {
			moduleName := filepath.Base(filepath.Dir(path))

			// Parse from package.json and set moduleName if successful
			var nodeModule NodeManifest
			if err := files.ReadJSON(&nodeModule, path); err == nil {
				moduleName = nodeModule.Name
			}

			log.Logger.Debugf("Found NodeJS package: %s (%s)", path, moduleName)
			path, _ = filepath.Rel(dir, path)
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: moduleName,
				Path: path,
				Type: string(module.Nodejs),
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find NodeJS package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}
