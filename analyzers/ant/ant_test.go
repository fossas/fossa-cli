package ant_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/ant"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDiscoveryRoot(t *testing.T) {
	modules, err := ant.Discover("testdata/valid-ant", make(map[string]interface{}))
	assert.Nil(t, err)
	assert.Equal(t, 2, len(modules))
	assert.Contains(t, modules, module.Module{
		Name:        "valid-ant",
		Type:        pkg.Ant,
		BuildTarget: ".",
		Dir:         ".",
	})
	assert.Contains(t, modules, module.Module{
		Name:        "valid-sub-dir",
		Type:        pkg.Ant,
		BuildTarget: "valid-sub-dir",
		Dir:         "valid-sub-dir",
	})
}
