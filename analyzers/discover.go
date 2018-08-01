package analyzers

import (
	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/analyzers/golang"

	"github.com/fossas/fossa-cli/module"
)

func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module

	antModules, err := ant.Discover(dir, options)
	if err != nil {
		return nil, err
	}
	modules = append(modules, antModules...)

	goModules, err := golang.Discover(dir, options)
	if err != nil {
		return nil, err
	}
	modules = append(modules, goModules...)

	return modules, nil
}
