// Package rust provides analysers for Rust projects.
//
// A `BuildTarget` in Rust is the directory of the Rust project containing `Cargo.lock`.

package rust

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/cargo"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Module  module.Module
	Options Options
}

type Options struct {
	Strategy     string `mapstructure:"strategy"`
	LockfilePath string `mapstructure:"cargo-lock"`
}

func New(m module.Module) (*Analyzer, error) {
	log.WithField("options", m.Options).Debug("constructing analyzer")

	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("parsed analyzer options")

	return &Analyzer{
		Module:  m,
		Options: options,
	}, nil
}

// Discover constructs modules in all directories with a `Cargo.lock`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
		}

		if !info.IsDir() && info.Name() == "Cargo.lock" {
			moduleName := filepath.Base(path)

			log.WithFields(log.Fields{
				"path": path,
				"name": moduleName,
			}).Debug("found Rust module")
			relPath, _ := filepath.Rel(dir, path)
			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Rust,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Rust package manifests: %s", err.Error())
	}
	return modules, nil
}

// Clean logs a warning and does nothing for Rust.
func (a *Analyzer) Clean() error {
	log.Warnf("Clean is not implemented for Rust")
	return nil
}

func (a *Analyzer) Build() error {
	return errors.New("Build not implemented for Rust")
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return files.Exists(a.Module.Dir, "Cargo.lock")
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	// Fallbacks
	// 1. Try to run `cargo tree` if it exists.
	// 2. Read Cargo.lock with Cargo.toml for direct deps.
	// 3. Read Cargo.lock for all deps.
	// TODO 4. Read only Cargo.tomls. Not sure how to implement this
	// without file tree traversal.
	switch a.Options.Strategy {
	case "manifest":
		fallthrough
		// return cargo.ManifestDependencies()
	case "lockfile":
		fallthrough
	default:
		return cargo.LockfileDependencies("Cargo.lock", a.Module.Dir)
	}
}
