package config

// ModuleType is an enumeration of supported build system types
type ModuleType string

const (
	// Individual tools

	// Bower is the module type for bower.io
	Bower = ModuleType("bower")
	// Composer is the module type for getcomposer.org
	Composer = ModuleType("composer")
	// Maven is the module type for maven.apache.org
	Maven = ModuleType("maven")
	// SBT is the module type for scala-sbt.org
	SBT = ModuleType("sbt")

	// Ecosystems where many tools behave similarly

	// Ruby is the module type for Bundler (bundler.io)
	Ruby = ModuleType("ruby")
	// Nodejs is the module type for NPM (npmjs.org) and Yarn (yarnpkg.com)
	Nodejs = ModuleType("nodejs")
	// Golang is the module type for dep, glide, godep, govendor, vndr, and manual
	// gopath vendoring
	Golang = ModuleType("golang")

	// VendoredArchives is a module type for archive formats (.tar, .rpm, .zip, etc...)
	VendoredArchives = ModuleType("vendoredarchives")
)

// GetModuleType returns a ModuleType for a variety of config keys
func GetModuleType(configKey string) ModuleType {
	switch configKey {
	// Node aliases
	case "commonjspackage":
		fallthrough
	case "nodejs":
		return Nodejs

	// Bower aliases
	case "bower":
		return Bower

	// Compower aliases
	case "composer":
		return Composer

	// Golang aliases
	case "gopackage":
		fallthrough
	case "golang":
		fallthrough
	case "go":
		return Golang

	// Maven aliases
	case "maven":
		fallthrough
	case "mvn":
		return Maven

	// Ruby aliases
	case "bundler":
		fallthrough
	case "gem":
		fallthrough
	case "rubygems":
		fallthrough
	case "ruby":
		return Ruby

	// SBT aliases
	case "scala":
		fallthrough
	case "sbtpackage":
		fallthrough
	case "sbt":
		return SBT

	// Archive aliases
	case "vendoredarchives":
		return VendoredArchives
	}
	return ""
}
