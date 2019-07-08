// Package rust provides analysers for Rust projects.
//
// A `BuildTarget` in Rust is the directory of the Rust project containing `Cargo.lock`.

package rust

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/cargo"
	"github.com/fossas/fossa-cli/errors"
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
	Strategy string `mapstructure:"strategy"`
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
			log.WithFields(log.Fields{
				"path": path,
				"name": path,
			}).Debug("found Rust module")
			relPath, err := filepath.Rel(dir, path)
			if err != nil {
				return errors.Wrap(err, "error discovering rust modules")
			}
			modules = append(modules, module.Module{
				Name:        path,
				Type:        pkg.Rust,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}
		return nil
	})
	if err != nil {
		return nil, errors.Wrap(err, "Could not find Rust package manifests:")
	}

	return modules, nil
}

// Clean logs a warning and does nothing for Rust.
func (a *Analyzer) Clean() error {
	log.Warnf("Clean is not implemented for Rust")
	return nil
}

func (a *Analyzer) Build() error {
	return errors.New("Build is not implemented for Rust")
}

func (a *Analyzer) IsBuilt() (bool, error) {
	return files.Exists(a.Module.Dir, "Cargo.lock")
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	switch a.Options.Strategy {
	case "manifest":
		// TODO: Add for filetree scanning.
		fallthrough
	case "cargo-tree":
		// TODO: Check for `cargo-tree` command and parse output.
		// This will give the most accurate results.
		fallthrough
	case "lockfile":
		fallthrough
	default:
		return cargo.LockfileDependencies("Cargo.lock", a.Module.Dir)
	}
}
