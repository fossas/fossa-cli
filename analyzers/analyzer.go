// Package analyzers defines analyzers for various package types.
package analyzers

import (
	"errors"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/bower"
	"github.com/fossas/fossa-cli/analyzers/cocoapods"
	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/analyzers/nuget"
	"github.com/fossas/fossa-cli/analyzers/php"
	"github.com/fossas/fossa-cli/analyzers/python"
	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/analyzers/scala"

	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Errors that occur when loading analyzers.
var (
	ErrUnknownPackageType     = errors.New("could not find analyzer for package type")
	ErrAnalyzerNotImplemented = errors.New("analyzer is not implemented for package type")
)

// An Analyzer is an implementation of functionality for different build systems.
type Analyzer interface {
	Discover(dir string) ([]module.Module, error) // Finds modules in a given directory.

	// These methods all make best-effort attempts.
	Clean(m module.Module) error           // Cleans build artifacts.
	Build(m module.Module) error           // Builds the module.
	IsBuilt(m module.Module) (bool, error) // Checks whether a module has been built.

	Analyze(m module.Module) (module.Module, error) // Runs an analysis of a module.
}

// New returns the analyzer for any given package type.
func New(key pkg.Type, options map[string]interface{}) (Analyzer, error) {
	switch key {
	case pkg.Ant:
		return ant.New(options)
	case pkg.Bower:
		return bower.New(options)
	case pkg.Cocoapods:
		return cocoapods.New(options)
	case pkg.Composer:
		return php.New(options)
	case pkg.Go:
		return golang.New(options)
	case pkg.Gradle:
		return gradle.New(options)
	case pkg.Maven:
		return maven.New(options)
	case pkg.NodeJS:
		return nodejs.New(options)
	case pkg.NuGet:
		return nuget.New(options)
	case pkg.Python:
		return python.New(options)
	case pkg.Ruby:
		return ruby.New(options)
	case pkg.Scala:
		return scala.New(options)
	}
	return nil, ErrUnknownPackageType
}

// TODO: it probably makes more sense for analyzers to contain a module (because
// some analyzer.New() methods work better with e.g. m.Dir or m.BuildTarget for
// setting up tools -- alternatively, they'd need an a.Initialize(m)), and for
// Discover to be implemented separately.
// type Analyzer interface {
// 	// These methods all make best-effort attempts.
// 	Clean() error           // Cleans build artifacts.
// 	Build() error           // Builds the module.
// 	IsBuilt() (bool, error) // Checks whether a module has been built.

// 	Analyze() (graph.Deps, error) // Runs an analysis of a module.
// }

// func New(m *module.Module) (Analyzer, error) {}

// func Discover(dir string) ([]module.Module, error) {}
