package module

// Type is an enumeration of supported build system types
type Type string

const (
	// Individual tools

	// Bower is the module type for bower.io
	Bower = Type("bower")
	// Composer is the module type for getcomposer.org
	Composer = Type("composer")
	// Maven is the module type for maven.apache.org
	Maven = Type("maven")
	// Sbt is the module type for scala-sbt.org
	Sbt = Type("sbt")

	// Ecosystems where many tools behave similarly

	// Ruby is the module type for Bundler (bundler.io)
	Ruby = Type("ruby")
	// Nodejs is the module type for NPM (npmjs.org) and Yarn (yarnpkg.com)
	Nodejs = Type("nodejs")
	// Golang is the module type for dep, glide, godep, govendor, vndr, and manual
	// gopath vendoring
	Golang = Type("golang")
)

// A Module is a unit of buildable code within a project.
type Module struct {
	Name string
	Type Type
	// Target is the entry point or manifest path for the build system. The exact
	// meaning is Type-dependent.
	Target string
	// Dir is the absolute path to the module's working directory (the directory
	// you would normally run the build command from).
	Dir string
}

// A Builder is an implementation of functionality for different build systems.
type Builder interface {
	// Initialize gathers build environment information and does build setup.
	Initialize() error
	// Build runs a best-effort attempt at building the module.
	Build(m Module, force bool) error
	// Analyze returns the dependencies of a module.
	Analyze(m Module) ([]Dependency, error)

	// IsBuilt checks whether a module has been built.
	IsBuilt(m Module) (bool, error)
	// IsModule checks whether a build target is a valid module. This is used for
	// inferring default configurations.
	IsModule(target string) (bool, error)
	InferModule(target string) (Module, error)
}
