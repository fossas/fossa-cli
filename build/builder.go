package build

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

	// IsModule takes a configKey and returns whether it is valid to elect this Builder.
	IsModule(configKey string) (bool, error)
	// DiscoverModules finds what modules are available for analysis in a given directory.
	DiscoverModules(dir string) ([]config.ModuleConfig, error)
}
