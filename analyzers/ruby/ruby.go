// Package ruby provides analysers for Ruby projects.
//
// A `BuildTarget` in Ruby is the directory of the Ruby project, generally
// containing `Gemfile` and `Gemfile.lock`.
package ruby

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/bundler"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// TODO: add a Ruby sidecar that evaluates `Gemfile` and `*.gemspec`.

type Analyzer struct {
	RubyCmd     string
	RubyVersion string

	BundlerCmd     string
	BundlerVersion string

	Bundler bundler.Bundler
	Module  module.Module
	Options Options
}

type Options struct {
	Strategy     string `mapstructure:"strategy"`
	LockfilePath string `mapstructure:"gemfile-lock-path"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debug("%#v", m.Options)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	// Construct analyzer.
	rubyCmd, rubyVersion, err := exec.Which("--version", os.Getenv("FOSSA_RUBY_CMD"), "ruby")
	if err != nil {
		log.Logger.Warningf("Could not resolve Ruby")
	}
	bundlerCmd, bundlerVersion, err := exec.Which("--version", os.Getenv("FOSSA_BUNDLER_CMD"), "bundler", "bundle")
	if err != nil {
		log.Logger.Warningf("Could not resolve Bundler")
	}
	return &Analyzer{
		RubyCmd:     rubyCmd,
		RubyVersion: rubyVersion,

		BundlerCmd:     bundlerCmd,
		BundlerVersion: bundlerVersion,

		Bundler: bundler.Bundler{
			Cmd: bundlerCmd,
		},
		Module:  m,
		Options: options,
	}, nil
}

// Discover constructs modules in all directories with a `Gemfile`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
		}

		if !info.IsDir() && info.Name() == "Gemfile" {
			moduleName := filepath.Base(path)

			log.Logger.Debugf("Found Ruby package: %s (%s)", path, moduleName)
			relPath, _ := filepath.Rel(dir, path)
			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Ruby,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Ruby package manifests: %s", err.Error())
	}
	return modules, nil
}

// Clean logs a warning and does nothing for Ruby.
func (a *Analyzer) Clean() error {
	// TODO: maybe this should delete `vendor/` for `bundle --deployment`
	// installations? How would we detect that?
	log.Logger.Warningf("Clean is not implemented for Ruby")
	return nil
}

func (a *Analyzer) Build() error {
	return a.Bundler.Install()
}

func (a *Analyzer) IsBuilt() (bool, error) {
	_, err := a.Bundler.List()
	if err != nil {
		return false, err
	}
	return true, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	lockfilePath := filepath.Join(a.Module.Dir, "Gemfile.lock")
	if a.Options.LockfilePath != "" {
		lockfilePath = a.Options.LockfilePath
	}

	switch a.Options.Strategy {
	case "list":
		gems, err := a.Bundler.List()
		if err != nil {
			return graph.Deps{}, err
		}
		imports, deps := FromGems(gems)
		return graph.Deps{
			Direct:     imports,
			Transitive: deps,
		}, nil
	case "lockfile":
		lockfile, err := bundler.FromLockfile(lockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
		imports, deps := FromLockfile(lockfile)
		return graph.Deps{
			Direct:     imports,
			Transitive: deps,
		}, nil
	case "list-lockfile":
		fallthrough
	default:
		lockfile, err := bundler.FromLockfile(lockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
		gems, err := a.Bundler.List()
		if err != nil {
			return graph.Deps{}, err
		}
		imports, deps := FilteredLockfile(gems, lockfile)
		return graph.Deps{
			Direct:     imports,
			Transitive: deps,
		}, nil
	}
}
