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
