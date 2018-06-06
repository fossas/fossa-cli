package builders

import (
	"github.com/fossas/fossa-cli/builders/ant"
	"github.com/fossas/fossa-cli/builders/archive"
	"github.com/fossas/fossa-cli/builders/bower"
	"github.com/fossas/fossa-cli/builders/cocoapods"
	"github.com/fossas/fossa-cli/builders/golang"
	"github.com/fossas/fossa-cli/builders/gradle"
	"github.com/fossas/fossa-cli/builders/maven"
	"github.com/fossas/fossa-cli/builders/nodejs"
	"github.com/fossas/fossa-cli/builders/nuget"
	"github.com/fossas/fossa-cli/builders/php"
	"github.com/fossas/fossa-cli/builders/python"
	"github.com/fossas/fossa-cli/builders/ruby"
	"github.com/fossas/fossa-cli/builders/scala"
)

// New instantiates a Builder given a ModuleType
func New(moduleType pkg.Type) pkg.Builder {
	switch moduleType {
	case pkg.Ant:
		return &ant.AntBuilder{}
	case pkg.Bower:
		return &bower.BowerBuilder{}
	case pkg.Cocoapods:
		return &cocoapods.CocoapodsBuilder{}
	case pkg.Composer:
		return &php.ComposerBuilder{}
	case pkg.Golang:
		return &golang.GoBuilder{}
	case pkg.Gradle:
		return &gradle.GradleBuilder{}
	case pkg.Maven:
		return &maven.MavenBuilder{}
	case pkg.Nodejs:
		return &nodejs.NodeJSBuilder{}
	case pkg.NuGet:
		return &nuget.NuGetBuilder{}
	case pkg.Pip:
		return &python.PipBuilder{}
	case pkg.Ruby:
		return &ruby.RubyBuilder{}
	case pkg.SBT:
		return &scala.SBTBuilder{}
	case pkg.VendoredArchives:
		return &archive.VendoredArchiveBuilder{}
	}
	return nil
}
