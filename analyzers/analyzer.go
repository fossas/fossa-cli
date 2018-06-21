// Package analyzers defines analyzers for various package types.
package analyzers

import (
	"errors"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/analyzers/python"

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
	Clean(config module.Module) error           // Cleans build artifacts.
	Build(config module.Module) error           // Builds the module.
	IsBuilt(config module.Module) (bool, error) // Checks whether a module has been built.

	Analyze(config module.Module) (module.Module, error) // Runs an analysis of a module.
}

// New returns the analyzer for any given package type.
func New(key pkg.Type, options map[string]interface{}) (Analyzer, error) {
	switch key {
	case pkg.Ant:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Bower:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Cocoapods:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Composer:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Go:
		return golang.New(options)
	case pkg.Gradle:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Maven:
		return nil, ErrAnalyzerNotImplemented
	case pkg.NodeJS:
		return nodejs.New(options)
	case pkg.NuGet:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Python:
		return python.New(options)
	case pkg.Ruby:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Scala:
		return nil, ErrAnalyzerNotImplemented
	case pkg.VendoredArchives:
		return nil, ErrAnalyzerNotImplemented
	}
	return nil, ErrUnknownPackageType
}
