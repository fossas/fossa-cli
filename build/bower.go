package build

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var bowerLogger = logging.MustGetLogger("bower")

// BowerDependency implements Dependency for BowerBuilder.
type BowerDependency struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns bower for BowerDependency. TODO: Support git and other
// dependency sources.
func (m BowerDependency) Fetcher() string {
	return "bower" // TODO: support git and etc...
}

// Package returns the package name for BowerDependency
func (m BowerDependency) Package() string {
	return m.Name
}

// Revision returns the version for BowerDependency
func (m BowerDependency) Revision() string {
	return m.Version
}

// BowerBuilder implements Builder for Bower.
// These properties are public for the sake of serialization.
type BowerBuilder struct {
	NodeCmd     string
	NodeVersion string

	BowerCmd     string
	BowerVersion string
}

// Initialize collects environment data for Bower builds
func (builder *BowerBuilder) Initialize() error {
	bowerLogger.Debugf("Initializing Bower builder...\n")
	// Set Node context variables
	nodeCmds := [3]string{os.Getenv("NODE_BINARY"), "node", "nodejs"}
	for i := 0; true; i++ {
		if i >= len(nodeCmds) {
			return errors.New("could not find Nodejs binary (try setting $NODE_BINARY)")
		}
		if nodeCmds[i] == "" {
			continue
		}

		nodeVersionOutput, err := exec.Command(nodeCmds[i], "-v").Output()
		if err == nil && nodeVersionOutput[0] == 'v' {
			builder.NodeVersion = strings.TrimSpace(string(nodeVersionOutput))[1:]
			builder.NodeCmd = nodeCmds[i]
			break
		}
	}

	// Set Bower context variables
	builder.BowerCmd = os.Getenv("Bower_BINARY")
	if builder.BowerCmd == "" {
		builder.BowerCmd = "bower"
	}

	bowerVersionOutput, err := exec.Command(builder.BowerCmd, "-v").Output()
	if err == nil && len(bowerVersionOutput) >= 5 {
		builder.BowerVersion = strings.TrimSpace(string(bowerVersionOutput))
	}

	if builder.BowerCmd == "" || builder.BowerVersion == "" {
		return errors.New("could not find Bower binary (try setting $BOWER_BINARY)")
	}

	bowerLogger.Debugf("Initialized Bower builder: %+v\n", builder)

	return nil
}

func (builder *BowerBuilder) Build(m module.Module, force bool) error {
	bowerLogger.Debugf("Running Bower build...\n")
	if force {
		bowerLogger.Debug("`force` flag is set; clearing `bower_components`...\n")
		cmd := exec.Command("rm", "-rf", "bower_components")
		cmd.Dir = m.Dir
		_, err := cmd.Output()
		if err != nil {
			return err
		}
	}

	cmd := exec.Command(builder.BowerCmd, "install", "--production")
	cmd.Dir = m.Dir
	_, err := cmd.Output()
	return err
}

func (builder *BowerBuilder) Analyze(m module.Module) ([]module.Dependency, error) {
	bowerLogger.Debugf("Running analysis on Bower module...\n")
	bowerComponents, err := doublestar.Glob(filepath.Join(m.Dir, "**", "bower_components", "*", ".bower.json"))
	if err != nil {
		return nil, err
	}
	bowerLogger.Debugf("Found %d modules from globstar.\n", len(bowerComponents))

	var wg sync.WaitGroup
	dependencies := make([]BowerDependency, len(bowerComponents))
	wg.Add(len(bowerComponents))

	for i := 0; i < len(bowerComponents); i++ {
		go func(modulePath string, index int, wg *sync.WaitGroup) {
			defer wg.Done()

			dependencyManifest, err := ioutil.ReadFile(modulePath)
			if err != nil {
				bowerLogger.Warningf("Error parsing Module: %s\n", modulePath)
				return
			}

			// Write directly to a reserved index for thread safety
			json.Unmarshal(dependencyManifest, &dependencies[index])
		}(bowerComponents[i], i, &wg)
	}

	wg.Wait()

	var deps []module.Dependency
	for i := 0; i < len(dependencies); i++ {
		deps = append(deps, dependencies[i])
	}

	return deps, nil
}

func (builder *BowerBuilder) IsBuilt(m module.Module) (bool, error) {
	bowerComponentsPath := filepath.Join(m.Dir, "bower_components")
	bowerLogger.Debugf("Checking bower_components at %s\n", bowerComponentsPath)
	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	if _, err := os.Stat(bowerComponentsPath); err == nil {
		return true, nil
	}
	return false, nil
}

func (builder *BowerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for BowerBuilder")
}

func (builder *BowerBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for BowerBuilder")
}
