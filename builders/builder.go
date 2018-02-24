package builders

import (
	"github.com/fossas/fossa-cli/config"
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
	DiscoverModules(dir string) ([]config.ModuleConfig, error)
}

// New instantiates a Builder given a ModuleType
func New(moduleType config.ModuleType) Builder {
	switch moduleType {
	case config.Bower:
		return &BowerBuilder{}
	case config.Composer:
		return &ComposerBuilder{}
	case config.Golang:
		return &GoBuilder{}
	case config.Maven:
		return &MavenBuilder{}
	case config.Nodejs:
		return &NodeJSBuilder{}
	case config.Ruby:
		return &RubyBuilder{}
	case config.SBT:
		return &SBTBuilder{}
	case config.VendoredArchives:
		return &VendoredArchiveBuilder{}
	}
	return nil
}
