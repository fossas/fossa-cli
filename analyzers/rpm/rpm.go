package rpm

import (
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/buildtools/rpm"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/mitchellh/mapstructure"
)

type Analyzer struct {
	Module  module.Module
	RPM     rpm.Shell
	Options Options
}
type Options struct {
	Strategy string `mapstructure:"strategy"`
}

func Discover(dir string, option map[string]interface{}) ([]module.Module, error) {
	return nil, nil
}

func New(m module.Module) (*Analyzer, error) {
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("parsed analyzer options")

	return &Analyzer{
		Module: m,

		Options: options,
		RPM:     rpm.ShellOutput(),
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

// Analyze for RPM takes a user specified rpm package and uploads all transitive dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	if a.Module.BuildTarget == "." || a.Options.Strategy == "system" {
		return a.RPM.SystemPackages()
	}
	return a.RPM.SinglePackage(a.Module.BuildTarget)
}
