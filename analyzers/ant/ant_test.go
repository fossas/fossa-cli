package ant_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDiscoveryRoot(t *testing.T) {
	modules, err := ant.Discover("testdata/valid", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, 1, len(modules))
	assert.Contains(t, modules, module.Module{
		Name:        "valid",
		Type:        pkg.Ant,
		BuildTarget: "testdata/valid",
		Dir:         "testdata/valid",
	})
}
func TestDiscoveryEmptyRoot(t *testing.T) {
	modules, err := ant.Discover("testdata", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Equal(t, 1, len(modules))
	assert.Contains(t, modules, module.Module{
		Name:        "valid",
		Type:        pkg.Ant,
		BuildTarget: "testdata/valid",
		Dir:         "testdata/valid",
	})
}
