package pkg

import "errors"

// NOTE: please keep these lists in alphabetical order.

// The Type of a Package indicates the ecosystem of the package. Generally, this
// corresponds to tool, registry, or language.
type Type int

// Supported package types.
const (
	Ant              Type = iota // Apache Ant (https://ant.apache.org)
	Bower                        // Bower (https://bower.io)
	Cocoapods                    // Cocoapods (https://cocoapods.org)
	Composer                     // Composer (https://getcomposer.org)
	Go                           // dep (https://github.com/golang/dep), glide (https://github.com/Masterminds/glide), godep (https://github.com/tools/godep), govendor (experimental, https://github.com/kardianos/govendor), vndr (https://github.com/LK4D4/vndr)
	Gradle                       // Gradle (https://gradle.org)
	Maven                        // Maven (https://maven.apache.org)
	NodeJS                       // NPM (https://www.npmjs.com), Yarn (https://yarnpkg.com)
	NuGet                        // NuGet (https://www.nuget.org)
	Python                       // Pip (https://pip.pypa.io)
	Ruby                         // Bundler (https://bundler.io)
	Scala                        // SBT (https://www.scala-sbt.org)
	VendoredArchives             // EXPERIMENTAL
)

// AllTypes enumerates all package types.
var AllTypes = []Type{
	Ant,
	Bower,
	Cocoapods,
	Composer,
	Go,
	Gradle,
	Maven,
	NodeJS,
	NuGet,
	Python,
	Ruby,
	Scala,
	VendoredArchives,
}

// Parse returns the canonical package type given a string key.
func Parse(key string) (Type, error) {
	switch key {
	// Ant aliases
	case "ant":
		return Ant, nil

	// Bower aliases
	case "bowerpackage":
		fallthrough
	case "bower":
		return Bower, nil

	// Cocoapods aliases
	case "ios":
		fallthrough
	case "pod":
		fallthrough
	case "cocoapodspackage":
		fallthrough
	case "cocoapods":
		return Cocoapods, nil

	// Composer aliases
	case "composerpackage":
		fallthrough
	case "composer":
		return Composer, nil

	// Go aliases
	case "gopackage":
		fallthrough
	case "golang":
		fallthrough
	case "go":
		return Go, nil

	// Gradle aliases
	case "gradle":
		return Gradle, nil

	// Maven aliases
	case "javaartifact":
		fallthrough
	case "maven":
		fallthrough
	case "mvn":
		return Maven, nil

	// NodeJS aliases
	case "commonjspackage":
		fallthrough
	case "npmpackage":
		fallthrough
	case "nodejs":
		return NodeJS, nil

	// NuGet aliases
	case "nugetpackage":
		fallthrough
	case "nuget":
		return NuGet, nil

	// Python aliases:
	case "python":
		fallthrough
	case "py":
		fallthrough
	case "pippackage":
		fallthrough
	case "pythonrequirementspackage":
		fallthrough
	case "pythonprogram":
		fallthrough
	case "pip":
		return Python, nil

	// Ruby aliases
	case "bundler":
		fallthrough
	case "gem":
		fallthrough
	case "rubygems":
		fallthrough
	case "ruby":
		return Ruby, nil

	// Scala aliases
	case "scala":
		fallthrough
	case "sbtpackage":
		fallthrough
	case "sbt":
		return Scala, nil

	// VendoredArchive aliases
	case "vendoredarchives":
		return VendoredArchives, nil
	}
	return Type(-1), errors.New("unknown package type")
}
