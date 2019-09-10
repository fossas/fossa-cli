// Package ruby provides analysers for Ruby projects.
//
// A `BuildTarget` in Ruby is the directory of the Ruby project, generally
// containing `Gemfile` and `Gemfile.lock`.
package ruby

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/bundler"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
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
	log.WithField("options", m.Options).Debug("constructing analyzer")

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.WithField("options", options).Debug("parsed analyzer options")

	// Construct analyzer.
	rubyCmd, rubyVersion, err := exec.Which("--version", os.Getenv("FOSSA_RUBY_CMD"), "ruby")
	if err != nil {
		log.Warnf("Could not resolve Ruby")
	}
	bundlerCmd, bundlerVersion, err := exec.Which("--version", os.Getenv("FOSSA_BUNDLER_CMD"), "bundler", "bundle")
	if err != nil {
		log.Warnf("Could not resolve Bundler")
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
			log.WithError(err).WithField("path", path).Debug("error while walking for discovery")
		}

		if !info.IsDir() && info.Name() == "Gemfile" {
			moduleName := filepath.Base(path)

			log.WithFields(log.Fields{
				"path": path,
				"name": moduleName,
			}).Debug("found Ruby module")
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
	log.Warnf("Clean is not implemented for Ruby")
	return nil
}

func (a *Analyzer) Build() error {
	return a.Bundler.Install()
}

func (a *Analyzer) IsBuilt() (bool, error) {
	ok, err := files.Exists(a.lockfilePath())
	if err != nil || !ok {
		return false, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: "Your ruby project may not be built, which will result in less accurate results. Generate a Gemfile.lock file by running `bundle install`",
			Link:            "https://bundler.io/v1.3/rationale.html",
		}
	}
	return true, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	strategy := "list-lockfile"

	if a.Options.Strategy != "" {
		strategy = a.Options.Strategy
	}

	lockfilePath := a.lockfilePath()

	switch strategy {
	case "list":
		return a.bundlerListAnalyzerStrategy()
	case "lockfile":
		return a.lockfileAnalyzerStrategy(lockfilePath)
	case "list-lockfile":
		fallthrough
	default:
		return a.bundlerListLockfileAnalyzerStrategy(lockfilePath)
	}
}

func (a *Analyzer) bundlerListLockfileAnalyzerStrategy(lockfilePath string) (graph.Deps, error) {
	lockfile, err := bundler.FromLockfile(lockfilePath)
	if err != nil {
		if a.Options.Strategy != "" {
			return graph.Deps{}, err
		}

		return a.lockfileAnalyzerStrategy(lockfilePath)
	}

	gems, err := a.Bundler.List()
	if err == nil {
		imports, deps := FilteredLockfile(gems, lockfile)

		return graph.Deps{
			Direct:     imports,
			Transitive: deps,
		}, nil
	}

	if a.Options.Strategy != "" {
		return graph.Deps{}, err
	}

	deps, err := a.lockfileAnalyzerStrategy(lockfilePath)
	if err == nil {
		return deps, err
	}

	if a.Options.Strategy != "" {
		return graph.Deps{}, err
	}

	return a.bundlerListAnalyzerStrategy()
}

func (a *Analyzer) bundlerListAnalyzerStrategy() (graph.Deps, error) {
	gems, err := a.Bundler.List()
	if err != nil {
		return graph.Deps{}, err
	}

	imports, deps := FromGems(gems)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func (a *Analyzer) lockfileAnalyzerStrategy(lockfilePath string) (graph.Deps, error) {
	lockfile, err := bundler.FromLockfile(lockfilePath)
	if err != nil {
		return graph.Deps{}, err
	}

	imports, deps := FromLockfile(lockfile)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func (a *Analyzer) lockfilePath() string {
	if a.Options.LockfilePath != "" {
		return a.Options.LockfilePath
	}
	return filepath.Join(a.Module.Dir, "Gemfile.lock")
}
