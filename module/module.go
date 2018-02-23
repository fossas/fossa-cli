package module

import "github.com/fossas/fossa-cli/config"

// A Module is a unit of buildable code within a project.
type Module struct {
	Name string
	Type config.ModuleType
	// Target is the entry point or manifest path for the build system. The exact
	// meaning is Type-dependent.
	Target string
	// Dir is the absolute path to the module's working directory (the directory
	// you would normally run the build command from).
	Dir string

	// TODO: add project and revision, because different modules might belong to
	// different projects
}

// A Builder is an implementation of functionality for different build systems.
type Builder interface {
	// Initialize gathers build environment information and does build setup.
	Initialize() error
	// Build runs a best-effort attempt at building the module.
	Build(m Module, force bool) error
	// Analyze returns the dependencies of a module.
	Analyze(m Module, allowUnresolved bool) ([]Dependency, error)

	// IsBuilt checks whether a module has been built.
	IsBuilt(m Module, allowUnresolved bool) (bool, error)
	// IsModule checks whether a build target is a valid module. This is used for
	// inferring default configurations.
	IsModule(target string) (bool, error)
	DiscoverModules(dir string) ([]config.ModuleConfig, error)
}
