package analyzers

import (
	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/bower"
	"github.com/fossas/fossa-cli/analyzers/cocoapods"
	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/analyzers/nuget"
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
		cocoapods.Discover,
		php.Discover,
		golang.Discover,
		gradle.Discover,
		maven.Discover,
		nodejs.Discover,
		nuget.Discover,
		python.Discover,
		ruby.Discover,
		scala.Discover,
	}

	for _, f := range discoverFuncs {
		discovered, err := f(dir, options)
		if err != nil {
			return nil, err
		}
		modules = append(modules, discovered...)
	}

	return modules, nil
}
