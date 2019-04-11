package pkg

import (
	"errors"
	"strings"
)

// NOTE: please keep these lists in alphabetical order.

// The Type of a Package indicates the ecosystem of the package. Generally, this
// corresponds to tool, registry, or language.
type Type int

// Supported package types.
const (
	Invalid   Type = iota // Placeholder
	Ant                   // Apache Ant (https://ant.apache.org)
	Bower                 // Bower (https://bower.io)
	Buck                  // Buck (https://buckbuild.com)
	Carthage              // Carthage (https://github.com/Carthage/Carthage)
	Cocoapods             // Cocoapods (https://cocoapods.org)
	Composer              // Composer (https://getcomposer.org)
	Debian                // Debian (https://manpages.debian.org/stretch/dpkg/dpkg.1.en.html)
	Go                    // dep (https://github.com/golang/dep), glide (https://github.com/Masterminds/glide), godep (https://github.com/tools/godep), govendor (experimental, https://github.com/kardianos/govendor), vndr (https://github.com/LK4D4/vndr), gomodules (https://github.com/golang/go/wiki/Modules)
	Git                   // git
	Gradle                // Gradle (https://gradle.org)
	Haskell               // Haskell with cabal-install (https://www.haskell.org/cabal/) or stack (https://www.haskellstack.org/)
	Maven                 // Maven (https://maven.apache.org)
	NodeJS                // NPM (https://www.npmjs.com), Yarn (https://yarnpkg.com)
	NuGet                 // NuGet (https://www.nuget.org)
	OkBuck                // OkBuck (https://github.com/uber/okbuck)
	Python                // Pip (https://pip.pypa.io), Pipenv (https://pipenv.readthedocs.io/en/latest/)
	Ruby                  // Bundler (https://bundler.io)
	Scala                 // SBT (https://www.scala-sbt.org)
	Raw                   // Unsupported languages
)

// AllTypes enumerates all package types.
var AllTypes = []Type{
	Ant,
	Bower,
	Buck,
	Carthage,
	Cocoapods,
	Composer,
	Debian,
	Go,
	Gradle,
	Haskell,
	Maven,
	NodeJS,
	NuGet,
	OkBuck,
	Python,
	Ruby,
	Scala,
	Raw,
}

// ParseType returns the canonical package type given a string key.
// TODO: if we got rid of aliases, we could use `go generate` with https://github.com/alvaroloes/enumer.
func ParseType(key string) (Type, error) {
	switch strings.ToLower(key) {
	// Ant aliases
	case "ant":
		return Ant, nil

	// Bower aliases
	case "bowerpackage":
		fallthrough
	case "bower":
		return Bower, nil

	// Buck aliases
	case "buck":
		return Buck, nil

	// Carthage aliases
	case "carthage":
		fallthrough
	case "cart":
		return Carthage, nil

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
	case "php":
		fallthrough
	case "comp":
		fallthrough
	case "composer":
		return Composer, nil

	case "debian":
		fallthrough
	case "dpkg":
		return Debian, nil

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

	// Haskell aliases
	case "stack":
		fallthrough
	case "cabal":
		fallthrough
	case "haskell":
		return Haskell, nil

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
	case "npm":
		fallthrough
	case "nodejs":
		return NodeJS, nil

	// NuGet aliases
	case "nugetpackage":
		fallthrough
	case "nuget":
		return NuGet, nil

	case "okbuck":
		return OkBuck, nil

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
		fallthrough
	case "pipenv":
		return Python, nil

	// Ruby aliases
	case "bundler":
		fallthrough
	case "gem":
		fallthrough
	case "rubygems":
		fallthrough
	case "rb":
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

	// Raw aliases
	case "c":
		fallthrough
	case "c++":
		fallthrough
	case "cpp":
		fallthrough
	case "tar":
		fallthrough
	case "tarball":
		fallthrough
	case "vendor":
		fallthrough
	case "vendored":
		fallthrough
	case "raw":
		return Raw, nil

	default:
		return Type(-1), errors.New("unknown package type")
	}
}

func (t Type) String() string {
	switch t {
	case Ant:
		return "ant"
	case Bower:
		return "bower"
	case Buck:
		return "buck"
	case Carthage:
		return "cart"
	case Cocoapods:
		return "pod"
	case Composer:
		return "composer"
	case Debian:
		return "debian"
	case Go:
		return "go"
	case Git:
		return "git"
	case Gradle:
		return "gradle"
	case Haskell:
		return "haskell"
	case Maven:
		return "mvn"
	case NodeJS:
		return "npm"
	case NuGet:
		return "nuget"
	case OkBuck:
		return "okbuck"
	case Python:
		return "pip"
	case Ruby:
		return "gem"
	case Scala:
		return "sbt"
	case Raw:
		return "archive"
	default:
		panic(t)
	}
}
