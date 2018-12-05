package buck

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"
	"gopkg.in/go-ini/ini.v1"

	"github.com/fossas/fossa-cli/buildtools/buck"
	"github.com/fossas/fossa-cli/exec"
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

// Analyze analyzers a buck package and its dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	return a.Cmd.Deps(a.Upload)
}

// Discover finds a Buck project by first looking for a ".buckconfig" file and then a "BUCK" file.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	var moduleList []module.Module

	_, err := os.Stat(".buckconfig")
	if err == nil {
		file, err := ini.Load(".buckconfig")
		if err != nil {
			return nil, errors.Errorf("Unable to read .buckconfig file: %s", err)
		}

		repositories, err := file.GetSection("repositories")
		// If no repositores are listed we are at the head of a cell. Attempt to use the base directory.
		if err != nil {
			wd, err := os.Getwd()
			if err != nil {
				return nil, errors.Errorf("Cannot get working directory: %s", err)
			}
			baseDirectory := filepath.Base(wd)
			bestGuessTarget := ":" + baseDirectory

			_, err = buckCmd("targets", bestGuessTarget)
			if err != nil {
				log.Warnf("Buck module %s's build target was incorrectly indentified.", baseDirectory)
			}

			return append(moduleList, newModule(baseDirectory, bestGuessTarget, ".")), nil
		}

		for name, dir := range repositories.KeysHash() {
			target := name
			aliases, err := file.GetSection("alias")
			if err == nil {
				alias, err := aliases.GetKey(name)
				if err == nil {
					target = alias.String()
				}
			}

			moduleList = append(moduleList, newModule(name, target, dir))
		}
		return moduleList, nil
	}

	_, err = os.Stat("BUCK")
	if err == nil {
		wd, err := os.Getwd()
		if err != nil {
			return nil, errors.Errorf("Cannot get working directory: %s", err)
		}

		buckRoot, err := buckCmd("root")
		if err != nil {
			return nil, err
		}

		// Condition the current directory to of format "parent/child:" from the root directory.
		buckDirectory := strings.TrimPrefix(wd, strings.TrimSpace(buckRoot)+"/")
		out, err := buckCmd("targets", buckDirectory+":")
		if err != nil {
			return nil, err
		}

		targets := strings.Split(out, "\n")
		for _, target := range targets {
			if len(target) > 0 {
				moduleList = append(moduleList, newModule(target, target, buckDirectory))
			}
		}

		return moduleList, nil
	}

	return nil, errors.Errorf("Could not find Buck config file: %s", err.Error())

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
