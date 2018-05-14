package module

import (
	"os"
	"path/filepath"
	"strings"
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

	// Different modules in a monolith may correspond to different FOSSA projects
	// or revisions.
	// TODO: use these values.
	Project  string
	Revision string

	// A catch-all for builders to add metadata (a la Context.Value).
	Context interface{}
}

// New instantiates and sets up a Module for a given ModuleType
func New(moduleType Type, conf Config) (Module, error) {
	var manifestName string
	var moduleTarget string

	// Find root dir of module
	modulePath, err := filepath.Abs(conf.Path)
	if err != nil {
		return Module{}, err
	}

	// infer default module settings from type
	switch moduleType {
	case Ant:
		manifestName = "build.xml"
		break
	case Bower:
		manifestName = "bower.json"
		break
	case Cocoapods:
		manifestName = "Podfile"
		break
	case Composer:
		manifestName = "composer.json"
		break
	case Golang:
		manifestName = ""
		moduleTarget = strings.TrimPrefix(modulePath, filepath.Join(os.Getenv("GOPATH"), "src")+"/")
		break
	case Maven:
		manifestName = "pom.xml"
		break
	case Nodejs:
		manifestName = "package.json"
		break
	case NuGet:
		moduleTarget = modulePath
		modulePath = filepath.Dir(modulePath)
		break
	case Pip:
		manifestName = "requirements.txt"
		break
	case Ruby:
		manifestName = "Gemfile"
		break
	case SBT:
		manifestName = "build.sbt"
		break
	case VendoredArchives:
		manifestName = ""
		break
	}

	// trim manifest from path
	if filepath.Base(modulePath) == manifestName {
		modulePath = filepath.Dir(modulePath)
	}

	moduleName := conf.Name
	if moduleName == "" {
		moduleName = conf.Path
	}

	if moduleTarget == "" {
		moduleTarget = filepath.Join(modulePath, manifestName)
	}

	return Module{
		Name:    moduleName,
		Type:    moduleType,
		Target:  moduleTarget,
		Dir:     modulePath,
		Context: conf.Options,
	}, nil
}
