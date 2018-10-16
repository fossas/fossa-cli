package nodejs_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

/* Nested Module Order
	└─┬ nested-modules
      └─┬ module-1
       	├── module-2
      	└── module-3
*/
func TestDiscoverNestedModules(t *testing.T) {
	var expectedModules []module.Module
	expectedModules = append(expectedModules, newModule("module-2", "module-1/module-2"))
	expectedModules = append(expectedModules, newModule("module-3", "module-1/module-3"))
	expectedModules = append(expectedModules, newModule("module-1", "module-1"))
	expectedModules = append(expectedModules, newModule("nested-modules", "."))

	modules, err := nodejs.Discover("testdata/nested-modules", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, expectedModules, modules)
}

/* Ignored Module Order
	└─┬ ignored-modules
      ├── node_modules
      └── bower_components
*/
func TestNodeModulesAndBowerIgnored(t *testing.T) {
	var expectedModules []module.Module
	expectedModules = append(expectedModules, newModule("ignored-modules", "."))

	modules, err := nodejs.Discover("testdata/ignored-modules", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, expectedModules, modules)
}

func newModule(name, location string) module.Module {
	return module.Module{
		Name:        name,
		Type:        pkg.NodeJS,
		BuildTarget: location,
		Dir:         location,
	}
}
