package module

import (
	"fmt"
)

// Config defines a config for a builder's entry point
type Config struct {
	Name    string  `yaml:"name"`
	Path    string  `yaml:"path"`
	Type    string  `yaml:"type"`
	Options Options `yaml:"options"`
}

type Options struct {
	Go     GoOptions
	Python PythonOptions
	Gradle GradleOptions
	Maven  MavenOptions
}

type GoOptions struct {
	AllowUnresolved bool
}

type PythonOptions struct {
	Strategy string
}

type GradleOptions struct {
	Task          string
	Configuration string
}

type MavenOptions struct {
	Settings string
}

type Analyzed struct {
	Module       Module
	Builder      Builder
	Dependencies []Dependency
}

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
	// SBT is the module type for scala-sbt.org
	SBT = Type("sbt")
	// Gradle is the module type for gradle.org
	Gradle = Type("gradle")
	// NuGet is the module type for nuget.org
	NuGet = Type("nuget")
	// Pip is the module type for https://pip.pypa.io/en/stable/
	Pip = Type("pip")

	// Ecosystems where many tools behave similarly

	// Ruby is the module type for Bundler (bundler.io)
	Ruby = Type("ruby")
	// Nodejs is the module type for NPM (npmjs.org) and Yarn (yarnpkg.com)
	Nodejs = Type("nodejs")
	// Golang is the module type for dep, glide, godep, govendor, vndr, and manual
	// gopath vendoring
	Golang = Type("golang")

	// VendoredArchives is a module type for archive formats (.tar, .rpm, .zip, etc...)
	VendoredArchives = Type("vendoredarchives")
)

// Types holds the list of all available module types for analysis
var Types = []Type{Bower, Composer, Maven, SBT, Gradle, NuGet, Pip, Ruby, Nodejs, Golang, VendoredArchives}

// Parse returns a module Type given a string
func Parse(key string) (Type, error) {
	switch key {
	// Node aliases
	case "commonjspackage":
		fallthrough
	case "nodejs":
		return Nodejs, nil

	// Bower aliases
	case "bower":
		return Bower, nil

	// Compower aliases
	case "composer":
		return Composer, nil

	// Golang aliases
	case "gopackage":
		fallthrough
	case "golang":
		fallthrough
	case "go":
		return Golang, nil

	// Maven aliases
	case "maven":
		fallthrough
	case "mvn":
		return Maven, nil

	// Python aliases:
	case "python":
		fallthrough
	case "py":
		fallthrough
	case "pip":
		return Pip, nil

	// Ruby aliases
	case "bundler":
		fallthrough
	case "gem":
		fallthrough
	case "rubygems":
		fallthrough
	case "ruby":
		return Ruby, nil

	// SBT aliases
	case "scala":
		fallthrough
	case "sbtpackage":
		fallthrough
	case "sbt":
		return SBT, nil

	case "gradle":
		return Gradle, nil

	case "nuget":
		return NuGet, nil

	// Archive aliases
	case "vendoredarchives":
		return VendoredArchives, nil
	}
	return "", fmt.Errorf("unknown module type: %s", key)
}
