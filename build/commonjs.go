package build

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"sync"

	zglob "github.com/mattn/go-zglob"

	. "github.com/fossas/fossa-cli/log"
)

// CommonJSContext implements build context for node_module & package.json builds
type CommonJSContext struct {
	NodeCmd     string
	NodeVersion string

	nodeModulesLocalDir       string
	nodeModulesGlobalDir      string
	isNodeModulesPrePopulated bool

	NpmCmd     string
	NpmVersion string

	npmShinkwrapFile string
	npmLockFile      string

	YarnCmd         string
	YarnVersion     string
	HasYarnLockFile bool
}

// CommonJSModule represents metadata from package.json files
type CommonJSModule struct {
	Name     string `json:"name"`
	Version  string `json:"version"`
	resolved bool
}

// Fetcher always returns npm for CommonJSModules
func (m CommonJSModule) Fetcher() string {
	return "npm" // TODO: support git and etc...
}

// Package returns the package spec for CommonJSModules
func (m CommonJSModule) Package() string {
	return m.Name
}

// Revision returns the version spec for CommonJSModules
func (m CommonJSModule) Revision() string {
	return m.Version
}

type commonJSBuildOpts struct {
	useCache bool
}

// Initialize collects environment data for CommonJS builds
func (ctx *CommonJSContext) Initialize(p *Module, opts map[string]interface{}) {
	// Set NodeJS context variables
	nodeCmds := [3]string{os.Getenv("NODE_BINARY"), "node", "nodejs"}

	for i := 0; i < len(nodeCmds) && ctx.NodeVersion == ""; i++ {
		if nodeCmds[i] != "" {
			outNodeVersion, err := exec.Command(nodeCmds[i], "-v").Output()
			if err == nil && outNodeVersion[0] == 'v' {
				ctx.NodeVersion = strings.TrimSpace(string(outNodeVersion))[1:]
				ctx.NodeCmd = nodeCmds[i]
				break
			}
		}
	}

	// Set YARN context variables
	if _, err := os.Stat("yarn.lock"); err == nil {
		ctx.HasYarnLockFile = true
	}

	ctx.YarnCmd = string(os.Getenv("YARN_BINARY"))
	if ctx.YarnCmd == "" {
		ctx.YarnCmd = "yarn"
	}
	outYarnVersion, err := exec.Command(ctx.YarnCmd, "-v").Output()
	if err == nil && len(outYarnVersion) >= 5 {
		ctx.YarnVersion = strings.TrimSpace(string(outYarnVersion))
	} else {
		ctx.YarnCmd = ""
		ctx.YarnVersion = ""
	}

	// Set NPM context variables
	ctx.NpmCmd = string(os.Getenv("NPM_BINARY"))
	if ctx.NpmCmd == "" {
		ctx.NpmCmd = "npm"
	}
	outNpmVersion, err := exec.Command(ctx.NpmCmd, "-v").Output()
	if err == nil && len(outNpmVersion) >= 5 {
		ctx.NpmVersion = strings.TrimSpace(string(outNpmVersion))
	} else {
		ctx.NpmCmd = ""
		ctx.NpmVersion = ""
	}

	ctx.isNodeModulesPrePopulated = ctx.verifyNodeModules()
}

// Verify checks if an install needs to be run
func (ctx *CommonJSContext) Verify(m *Module, opts map[string]interface{}) bool {
	return ctx.verifyNodeModules()
}

// Build determines and executes a CommonJS build based off available tooling in the environment
func (ctx *CommonJSContext) Build(m *Module, opts map[string]interface{}) error {
	if ctx.verifyNodeModules() == false || opts["no-cache"].(bool) == true {
		Log.Debug("No prebuilt node_modules directory, building...")
		if err := ctx.populateNodeModules(); err != nil {
			return err
		}
	} else {
		Log.Debug("Found pre-populated node_modules, skipping build...")
	}

	// Traverse node_modules directory and find all Modules
	// walk and parse all **/**/node_modules/Module.json
	// If node_modules is checked in directly, we don't need any env deps
	deps, err := ctx.traverseNodeModules()
	if err != nil {
		return err
	}

	m.Build.Dependencies = deps
	return nil
}

func (ctx *CommonJSContext) verifyNodeModules() bool {
	if _, err := os.Stat("node_modules"); err == nil {
		// check if node_modules directory looks kinda right
		// NOTE: we don't have great ways of doing this because there could be no deps
		return true
	} else {
		return false
	}
}

func (ctx *CommonJSContext) populateNodeModules() error {
	if ctx.NodeCmd == "" || ctx.NodeVersion == "" {
		return errors.New("No NodeJS installation detected. Try setting the $NODE_BINARY environment variable.")
	}

	if ctx.HasYarnLockFile {
		if ctx.YarnCmd != "" && ctx.YarnVersion != "" {
			// TODO(xizhao): Verify compatible yarn versions
			// yarn install
			exec.Command("yarn", "install", "--production").Output()

			// verify yarn build
			if ctx.verifyNodeModules() == false {
				Log.Warning("failed to run Yarn build... falling back to npm")
			} else {
				return nil
			}
		} else {
			Log.Warning("Yarn lockfile detected but no Yarn installation found. Try setting $YARN_BINARY environment variable; falling back to npm for analysis.")
		}
	}

	if ctx.NpmCmd == "" || ctx.NpmVersion == "" {
		return errors.New("No npm installation detected. Try setting the $NPM_BINARY environment variable.")
	}

	// npm install
	exec.Command("npm", "install", "--production").Output()
	if ctx.verifyNodeModules() == false {
		return errors.New("Failed to run npm build.")
	}
	return nil
}

func (ctx *CommonJSContext) traverseNodeModules() ([]Dependency, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	commonjsPkgs, err := zglob.Glob(cwd + "/**/node_modules/*/package.json")
	if err != nil {
		return nil, err
	}

	var wg sync.WaitGroup
	dependencies := make([]CommonJSModule, len(commonjsPkgs))

	for i := 0; i < len(commonjsPkgs); i++ {
		wg.Add(1)
		go func(path string, index int, wg *sync.WaitGroup) {
			defer wg.Done()
			rawPkg, err := ioutil.ReadFile(path)
			if err != nil {
				Log.Warningf("Error parsing Module: %s", path)
				return
			}
			// write directly to a reserved index for thread safety
			json.Unmarshal(rawPkg, &dependencies[index])
		}(commonjsPkgs[i], i, &wg)
	}

	wg.Wait()

	dedupedDependencies := []Dependency{}
	for _, d := range dependencies {
		dedupedDependencies = append(dedupedDependencies, Dependency(d))
	}

	return Dedupe(dedupedDependencies), nil
}
