package analyzers

import (
	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/bower"
	"github.com/fossas/fossa-cli/analyzers/cocoapods"
	"github.com/fossas/fossa-cli/analyzers/golang"

	"github.com/fossas/fossa-cli/module"
)

type discoverFunc func(dir string, options map[string]interface{}) ([]module.Module, error)

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	discoverFuncs := []discoverFunc{
		ant.Discover,
		bower.Discover,
		cocoapods.Discover,
		golang.Discover,
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
