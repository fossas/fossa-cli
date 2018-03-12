package module

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

	// IsModule takes a string module type and returns whether it matches to use this Builder.
	IsModule(key string) (bool, error)
	// DiscoverModules finds what modules are available for analysis in a given directory.
	DiscoverModules(dir string) ([]Config, error)
}
