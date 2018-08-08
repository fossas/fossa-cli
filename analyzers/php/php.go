// Package php implements analyzers for PHP.
//
// A `BuildTarget` for PHP is the path to the `composer.json`.
package php

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/composer"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Composer composer.Composer

	Module  module.Module
	Options Options
}

// TODO: strategies:
// - composer show
// - read lockfile
// - read manifest
// - read components

type Options struct {
	Strategy string `mapstructure:"strategy"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debug("%#v", m.Options)
	// Set Bower context variables
	composerCmd, _, err := exec.Which("--version", os.Getenv("COMPOSER_BINARY"), "composer")
	if err != nil {
		return nil, errors.Wrap(err, "could not find Composer binary (try setting $COMPOSER_BINARY)")
	}

	// Decode options
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		Composer: composer.Composer{
			Cmd: composerCmd,
		},

		Module:  m,
		Options: options,
	}

	log.Logger.Debugf("analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover finds `composer.json`s not a /vendor/ folder
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("failed to access path %s: %s\n", path, err.Error())
			return err
		}

		// Skip the /vendor/ folder
		if info.IsDir() && info.Name() == "vendor" {
			log.Logger.Debugf("skipping `vendor` directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "composer.json" {
			dir := filepath.Dir(path)
			name := filepath.Base(dir)

			// Parse from composer.json and set name if successful
			var composerPackage composer.Manifest
			if err := files.ReadJSON(&composerPackage, path); err == nil {
				name = composerPackage.Name
			}

			log.Logger.Debugf("found Composer package: %s (%s)", path, name)
			modules = append(modules, module.Module{
				Name:        name,
				Type:        pkg.Composer,
				BuildTarget: path,
				Dir:         dir,
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find Composer package manifests: %s", err.Error())
	}

	return modules, nil
}

func (a *Analyzer) Clean() error {
	return a.Composer.Clean(a.Module.Dir)
}

func (a *Analyzer) Build() error {
	return a.Composer.Install(a.Module.Dir)
}

func (a *Analyzer) IsBuilt() (bool, error) {
	_, err := a.Composer.Show(a.Module.Dir)
	if err != nil {
		return false, nil
	}
	return true, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	imports, deps, err := a.Composer.Dependencies(a.Module.Dir)
	if err != nil {
		return graph.Deps{}, err
	}

	var pkgImports []pkg.Import
	for _, i := range imports {
		pkgImports = append(pkgImports, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Composer,
				Name:     i.Name,
				Revision: i.Version,
			},
		})
	}

	g := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		id := pkg.ID{
			Type:     pkg.Composer,
			Name:     parent.Name,
			Revision: parent.Version,
		}
		var parentImports []pkg.Import
		for _, child := range children {
			parentImports = append(parentImports, pkg.Import{
				Resolved: pkg.ID{
					Type:     pkg.Composer,
					Name:     child.Name,
					Revision: child.Version,
				},
			})
		}
		g[id] = pkg.Package{
			ID:      id,
			Imports: parentImports,
		}
	}

	return graph.Deps{
		Direct:     pkgImports,
		Transitive: g,
	}, nil
}
