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
	"github.com/fossas/fossa-cli/log"
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
	Options Options
}

type Options struct {
	Strategy     string `mapstructure:"strategy"`
	LockfilePath string `mapstructure:"gemfile-lock-path"`
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debug("%#v", opts)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(opts, &options)
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
		Options: options,
	}, nil
}

// Discover constructs modules in all directories with a `Gemfile`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
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
func (a *Analyzer) Clean(m module.Module) error {
	// TODO: maybe this should delete `vendor/` for `bundle --deployment`
	// installations? How would we detect that?
	log.Logger.Warningf("Clean is not implemented for Ruby")
	return nil
}

func (a *Analyzer) Build(m module.Module) error {
	return a.Bundler.Install()
}

func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	_, err := a.Bundler.List()
	if err != nil {
		return false, err
	}
	return true, nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	lockfilePath := filepath.Join(m.Dir, "Gemfile.lock")
	if a.Options.LockfilePath != "" {
		lockfilePath = a.Options.LockfilePath
	}

	switch a.Options.Strategy {
	case "list":
		gems, err := a.Bundler.List()
		if err != nil {
			return m, err
		}
		imports, graph := FromGems(gems)
		m.Imports = imports
		m.Deps = graph
		return m, nil
	case "lockfile":
		lockfile, err := bundler.FromLockfile(lockfilePath)
		if err != nil {
			return m, err
		}
		imports, graph := FromLockfile(lockfile)
		m.Imports = imports
		m.Deps = graph
		return m, nil
	case "list-lockfile":
		fallthrough
	default:
		lockfile, err := bundler.FromLockfile(lockfilePath)
		if err != nil {
			return m, err
		}
		gems, err := a.Bundler.List()
		if err != nil {
			return m, err
		}
		imports, graph := FilteredLockfile(gems, lockfile)
		m.Imports = imports
		m.Deps = graph
		return m, nil
	}
}
