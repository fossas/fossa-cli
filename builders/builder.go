package builders

import (
	"github.com/fossas/fossa-cli/module"
)

// A Builder is an implementation of functionality for different build systems.
type Builder interface {
	// Initialize gathers build environment information and does build setup.
	Initialize() error
	// Build runs a best-effort attempt at building the module.
	Build(m module.Module, force bool) error
	// Analyze returns the dependencies of a module.
	Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error)

	// IsBuilt checks whether a module has been built.
	IsBuilt(m module.Module, allowUnresolved bool) (bool, error)

	// IsModule takes a string like and returns whether it matches to elect this Builder.
	IsModule(configKey string) (bool, error)
	// DiscoverModules finds what modules are available for analysis in a given directory.
	DiscoverModules(dir string) ([]module.Config, error)
}

// New instantiates a Builder given a ModuleType
func New(moduleType module.Type) Builder {
	switch moduleType {
	case module.Bower:
		return &BowerBuilder{}
	case module.Composer:
		return &ComposerBuilder{}
	case module.Golang:
		return &GoBuilder{}
	case module.Gradle:
		return &GradleBuilder{}
	case module.Maven:
		return &MavenBuilder{}
	case module.Nodejs:
		return &NodeJSBuilder{}
	case module.NuGet:
		return &NuGetBuilder{}
	case module.Pip:
		return &PipBuilder{}
	case module.Ruby:
		return &RubyBuilder{}
	case module.SBT:
		return &SBTBuilder{}
	case module.VendoredArchives:
		return &VendoredArchiveBuilder{}
	}
	return nil
}
