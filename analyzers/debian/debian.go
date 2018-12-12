package debian

import (
	"fmt"

	"github.com/fossas/fossa-cli/buildtools/debian"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
)

type Analyzer struct {
	Module module.Module
	Cmd    debian.Cmd
}

func Discover(dir string, option map[string]interface{}) ([]module.Module, error) {
	return nil, nil
}

func New(m module.Module) (*Analyzer, error) {

	return &Analyzer{
		Module: m,

		Cmd: debian.New(),
	}, nil
}

func (a *Analyzer) Clean() error {
	return nil
}

func (a *Analyzer) Build() error {
	return nil
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Analyze for Debian takes a user specified debian package and runs dpkg command
// Runs the same command on all deps and pulls out locators. Does a tarball upload similar to Buck
// Get deps, upload deps, create graph.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	fmt.Println(a.Module.BuildTarget, "module")
	return a.Cmd.Dependencies(a.Module.BuildTarget)
}
