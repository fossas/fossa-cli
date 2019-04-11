// Package analyzers defines analyzers for various package types.
package analyzers

import (
	"errors"
	"github.com/fossas/fossa-cli/analyzers/haskell"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/bower"
	"github.com/fossas/fossa-cli/analyzers/buck"
	"github.com/fossas/fossa-cli/analyzers/carthage"
	"github.com/fossas/fossa-cli/analyzers/cocoapods"
	"github.com/fossas/fossa-cli/analyzers/debian"
	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/analyzers/nuget"
	"github.com/fossas/fossa-cli/analyzers/okbuck"
	"github.com/fossas/fossa-cli/analyzers/php"
	"github.com/fossas/fossa-cli/analyzers/python"
	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/analyzers/scala"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Errors that occur when loading analyzers.
var (
	ErrUnknownModuleType      = errors.New("could not find analyzer for module type")
	ErrAnalyzerNotImplemented = errors.New("analyzer is not implemented for package type")
)

// An Analyzer is an implementation of functionality for different build
// systems.
type Analyzer interface {
	// These methods all make best-effort attempts.
	Clean() error           // Cleans build artifacts.
	Build() error           // Builds the module.
	IsBuilt() (bool, error) // Checks whether a module has been built.

	Analyze() (graph.Deps, error) // Runs an analysis of a module.
}

// New returns the analyzer for any given package type.
func New(m module.Module) (Analyzer, error) {
	switch m.Type {
	case pkg.Ant:
		return ant.New(m)
	case pkg.Bower:
		return bower.New(m)
	case pkg.Carthage:
		return carthage.New(m)
	case pkg.Cocoapods:
		return cocoapods.New(m)
	case pkg.Composer:
		return php.New(m)
	case pkg.Debian:
		return debian.New(m)
	case pkg.Go:
		return golang.New(m)
	case pkg.Gradle:
		return gradle.New(m)
	case pkg.Haskell:
		return haskell.New(m)
	case pkg.Maven:
		return maven.New(m)
	case pkg.NodeJS:
		return nodejs.New(m)
	case pkg.NuGet:
		return nuget.New(m)
	case pkg.OkBuck:
		return okbuck.New(m)
	case pkg.Python:
		return python.New(m)
	case pkg.Ruby:
		return ruby.New(m)
	case pkg.Scala:
		return scala.New(m)
	case pkg.Buck:
		return buck.New(m)
	}
	return nil, ErrUnknownModuleType
}
