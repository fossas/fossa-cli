package builders

import (
	"github.com/fossas/fossa-cli/module"

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
func New(moduleType module.Type) module.Builder {
	switch moduleType {
	case module.Ant:
		return &ant.AntBuilder{}
	case module.Bower:
		return &bower.BowerBuilder{}
	case module.Cocoapods:
		return &cocoapods.CocoapodsBuilder{}
	case module.Composer:
		return &php.ComposerBuilder{}
	case module.Golang:
		return &golang.GoBuilder{}
	case module.Gradle:
		return &gradle.GradleBuilder{}
	case module.Maven:
		return &maven.MavenBuilder{}
	case module.Nodejs:
		return &nodejs.NodeJSBuilder{}
	case module.NuGet:
		return &nuget.NuGetBuilder{}
	case module.Pip:
		return &python.PipBuilder{}
	case module.Ruby:
		return &ruby.RubyBuilder{}
	case module.SBT:
		return &scala.SBTBuilder{}
	case module.VendoredArchives:
		return &archive.VendoredArchiveBuilder{}
	}
	return nil
}
