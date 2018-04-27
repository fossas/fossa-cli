package builders

import (
	"github.com/fossas/fossa-cli/module"
)

// New instantiates a Builder given a ModuleType
func New(moduleType module.Type) module.Builder {
	switch moduleType {
	case module.Bower:
		return &BowerBuilder{}
	case module.Cocoapods:
		return &CocoapodsBuilder{}
	case module.Composer:
		return &ComposerBuilder{}
	case module.Golang:
		return &GoBuilder{}
	case module.Gradle:
		return &GradleBuilder{}
	case module.Maven:
		return &MavenBuilder{}
	case module.Nodejs:
		return &NodeJSBuilder{}
	case module.NuGet:
		return &NuGetBuilder{}
	case module.Pip:
		return &PipBuilder{}
	case module.Ruby:
		return &RubyBuilder{}
	case module.SBT:
		return &SBTBuilder{}
	case module.VendoredArchives:
		return &VendoredArchiveBuilder{}
	}
	return nil
}
