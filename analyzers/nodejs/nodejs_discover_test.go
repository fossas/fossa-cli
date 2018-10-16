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
func TestNestedModules(t *testing.T) {
	modules, err := nodejs.Discover("testdata/nested-modules", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 4)
	assert.Contains(t, modules, newModule("nested-modules", "."))
	assert.Contains(t, modules, newModule("module-1", "module-1"))
	assert.Contains(t, modules, newModule("module-2", "module-1/module-2"))
	assert.Contains(t, modules, newModule("module-3", "module-1/module-3"))
}

/* Ignored Module Order
	└─┬ ignored-modules
  	  ├── node_modules
  	  └── bower_components
*/
func TestNodeModulesAndBowerIgnored(t *testing.T) {
	modules, err := nodejs.Discover("testdata/ignored-modules", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 1)
	assert.Contains(t, modules, newModule("ignored-modules", "."))
}

func newModule(name, location string) module.Module {
	return module.Module{
		Name:        name,
		Type:        pkg.NodeJS,
		BuildTarget: location,
		Dir:         location,
	}
}
