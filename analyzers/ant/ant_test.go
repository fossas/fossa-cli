package ant_test

import (
	"testing"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestDiscovery(t *testing.T) {
	modules, err := ant.Discover("testdata", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, 1, len(modules))
	assert.Contains(t, modules, module.Module{
		Name: "valid",
		Type: pkg.Ant,
		Dir:  "testdata/valid",
	})
}
