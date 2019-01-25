// Package okbuck implements the analyzer for OkBuck. https://github.com/uber/okbuck.
//
// A `BuildTarget` in OkBuck is defined as a Build Target by OkBuck which is in
// in the format of `//src/build:target`. OkBuck defines this as a string used to
// identify a Build Rule.
//
// This package is implemented by externally calling the `okbuck` build tool.
//
// FAQ
//
// 1. Why is analyzing manifest files not a supported strategy as it is for other tools?
//
// `.buckconfig` can be used to discover cells but the `repository` field which
// defines cells is not required .
// `BUCK` files are written in Skylark (a dialect of Python) and are impossible to statically analyze.
// `buck audit` provides json formatted data for dependency and input information.
package okbuck

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/okbuck"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer defines a OkBuck analyzer.
type Analyzer struct {
	Module  module.Module
	Setup   okbuck.OkBuck
	Options Options
}

// Options sets analyzer options for Go modules.
type Options struct {
	ClassPath string `mapstructure:"classpath"` // Specify the classpath to target for a specific configurations dependencies.
}

// New constructs a new OkBuck analyzer from a module.
func New(module module.Module) (*Analyzer, error) {
	log.Debugf("%#v", module)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(module.Options, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("parsed analyzer options")

	analyzer := Analyzer{
		Module:  module,
		Setup:   okbuck.New(module.BuildTarget),
		Options: options,
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

// Analyze analyzes an OkBuck build target and its dependencies.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	return a.Setup.Deps(a.Options.ClassPath)
}

// Discover searches for `buckw` executables in the present directory.
func Discover(dir string, opts map[string]interface{}) ([]module.Module, error) {
	var moduleList []module.Module

	buckWrapper, err := files.Exists(dir, "buckw")
	if err != nil {
		return moduleList, err
	}
	if buckWrapper {
		wd, err := os.Getwd()
		if err != nil {
			return moduleList, err
		}
		moduleList = append(moduleList, newModule(filepath.Base(wd), "//...", "."))
	}

	return moduleList, nil
}

func newModule(name, target, dir string) module.Module {
	log.WithFields(log.Fields{
		"path": dir,
		"name": name,
	}).Debug("constructing OkBuck module")

	return module.Module{
		Name:        name,
		Type:        pkg.OkBuck,
		BuildTarget: target,
		Dir:         dir,
	}
}
