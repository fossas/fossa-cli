package analyzers

import (
	"github.com/apex/log"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/bower"
	"github.com/fossas/fossa-cli/analyzers/buck"
	"github.com/fossas/fossa-cli/analyzers/carthage"
	"github.com/fossas/fossa-cli/analyzers/cocoapods"
	"github.com/fossas/fossa-cli/analyzers/debian"
	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/analyzers/haskell"
	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/analyzers/nuget"
	"github.com/fossas/fossa-cli/analyzers/okbuck"
	"github.com/fossas/fossa-cli/analyzers/php"
	"github.com/fossas/fossa-cli/analyzers/python"
	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/analyzers/scala"
	"github.com/fossas/fossa-cli/module"
)

type discoverFunc func(dir string, options map[string]interface{}) ([]module.Module, error)

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	discoverFuncs := []discoverFunc{
		ant.Discover,
		bower.Discover,
		buck.Discover,
		carthage.Discover,
		cocoapods.Discover,
		debian.Discover,
		php.Discover,
		golang.Discover,
		gradle.Discover,
		haskell.Discover,
		maven.Discover,
		nodejs.Discover,
		nuget.Discover,
		okbuck.Discover,
		python.Discover,
		ruby.Discover,
		scala.Discover,
	}

	for _, f := range discoverFuncs {
		discovered, err := f(dir, options)
		if err != nil {
			log.Warnf("An error occurred during discovery: %s", err.Error())
		}
		modules = append(modules, discovered...)
	}

	return modules, nil
}
