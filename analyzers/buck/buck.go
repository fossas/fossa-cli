// Package buck implements the analyzer for Buck. https://buckbuild.com
//
// A `BuildTarget` in Buck is defined as a Build Target by Buck which is in
// in the format of `//src/build:target`. Buck defines this as a string used to
// identify a Build Rule.
//
// This package is implemented by externally calling the `buck` build tool.
//
// FAQ
//
// 1. Why is analyzing manifest files not a supported strategy as it is for other tools?
//
// `.buckconfig` can be used to discover cells but the `repository` field which
// defines cells is not required .
// `BUCK` files are written in Skylark (a dialect of Python) and are impossible to statically analyze.
// `buck audit` provides json formatted data for dependency and input information.
package buck

import (
	"os"
	"path"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"
	"gopkg.in/go-ini/ini.v1"

	"github.com/fossas/fossa-cli/buildtools/buck"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer defines a Buck analyzer.
type Analyzer struct {
	Module module.Module
	Upload bool
	Cmd    buck.Buck
}

// New constructs a new Buck analyzer from a module.
func New(module module.Module) (*Analyzer, error) {
	analyzer := Analyzer{
		Module: module,
		Upload: true,
		Cmd:    buck.New(module.BuildTarget),
	}
	return &analyzer, nil
}

// Clean is not implemented.
func (a *Analyzer) Clean() error {
	return nil
}

// Build is not implemented.
func (a *Analyzer) Build() error {
	return nil
}

// IsBuilt is not implemented.
func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Analyze analyzes a buck build target and its dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	return a.Cmd.Deps(a.Upload)
}

// Discover is used to operate Discovery with a custom `buck` command.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	return DiscoverWithCommand(dir, opts, buckCmd)
}

// DiscoverWithCommand finds a Buck project by first looking for a ".buckconfig" file and then a "BUCK" file.
// 1. ".buckconfig" file is found and we know that that we are at the root of a Buck cell.
// 	a. Attempt to find user defined aliases in .buckconfig.
// 	b. No aliases, run `buck targets //` to find all local targets.
// 2. "BUCK" file is found.
// 	a. Run `buck targets <directory>:` to find all local targets.
func DiscoverWithCommand(dir string, opts map[string]interface{}, buckCommand func(string, ...string) (string, error)) ([]module.Module, error) {
	var moduleList []module.Module
	buckConfig, err := files.Exists(dir, ".buckconfig")
	if err == nil && buckConfig {
		file, err := ini.Load(path.Join(dir, ".buckconfig"))
		if err != nil {
			return nil, errors.Errorf("Unable to read `.buckconfig`: %s", err)
		}

		aliases, err := file.GetSection("alias")
		if err == nil && len(aliases.Keys()) > 0 {
			for name, target := range aliases.KeysHash() {
				moduleList = append(moduleList, newModule(name, target, dir))
			}
			return moduleList, nil
		}

		out, err := buckCommand("targets", "//")
		if err != nil {
			return nil, err
		}

		buckTargetList := strings.Split(strings.TrimSpace(out), "\n")
		for _, target := range buckTargetList {
			moduleList = append(moduleList, newModule(target, target, dir))
		}
		return moduleList, nil
	}

	buckFile, err := files.Exists(dir, "BUCK")
	if err == nil && buckFile {
		wd, err := os.Getwd()
		if err != nil {
			return nil, errors.Errorf("Cannot get working directory: %s", err)
		}

		buckRoot, err := buckCommand("root")
		if err != nil {
			return nil, err
		}

		// Condition the current directory to the format of "parent/child:" from the root directory.
		buckDirectory := strings.TrimPrefix(wd, strings.TrimSpace(buckRoot)+"/")
		out, err := buckCommand("targets", buckDirectory+":")
		if err != nil {
			return nil, err
		}

		targets := strings.Split(out, "\n")
		for _, target := range targets {
			if len(target) > 0 {
				moduleList = append(moduleList, newModule(target, target, dir))
			}
		}
	}
	return moduleList, nil
}

func buckCmd(cmd string, args ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{cmd}, args...),
	})

	if err != nil {
		return out, errors.Wrapf(err, "Could not run `buck %s %+v` within the current directory", cmd, args)
	}
	return out, nil
}

func newModule(name, target, dir string) module.Module {
	log.WithFields(log.Fields{
		"path": dir,
		"name": name,
	}).Debug("constructing Buck module")

	return module.Module{
		Name:        name,
		Type:        pkg.Buck,
		BuildTarget: target,
		Dir:         dir,
	}
}
