// Package analyzers defines analyzers for various package types.
package analyzers

import (
	"errors"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/project"
)

// Errors that occur when loading analyzers.
var (
	ErrUnknownPackageType     = errors.New("could not find analyzer for package type")
	ErrAnalyzerNotImplemented = errors.New("analyzer is not implemented for package type")
)

// An Analyzer is an implementation of functionality for different build systems.
type Analyzer interface {
	Discover(dir string) ([]project.Project, error) // Finds projects in a given directory.

	// These methods all make best-effort attempts.
	Clean(config project.Project) error           // Cleans build artifacts.
	Build(config project.Project) error           // Builds the program.
	IsBuilt(config project.Project) (bool, error) // Checks whether a program has been built.

	Analyze(config project.Project) (project.Project, error) // Runs an analysis of a program.
}

// New returns the analyzer for any given package type.
func New(key pkg.Type, options interface{}) (Analyzer, error) {
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
		opts, ok := options.(project.GoOptions)
		if !ok {
			return nil, errors.New("golang analyzer requires GoOptions")
		}
		return golang.New(opts)
	case pkg.Gradle:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Maven:
		return nil, ErrAnalyzerNotImplemented
	case pkg.NodeJS:
		return nil, ErrAnalyzerNotImplemented
	case pkg.NuGet:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Python:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Ruby:
		return nil, ErrAnalyzerNotImplemented
	case pkg.Scala:
		return nil, ErrAnalyzerNotImplemented
	case pkg.VendoredArchives:
		return nil, ErrAnalyzerNotImplemented
	}
	return nil, ErrUnknownPackageType
}
