package init_test

import (
	"testing"

	initc "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestModuleFiltering(t *testing.T) {
	node := module.Module{Dir: "temp/node_modules/subdir"}
	nodeWindows := module.Module{Dir: "temp\\node_modules\\subdir"}
	temp := module.Module{Dir: "tmp/subdir"}
	goNotExecutable := module.Module{IsExecutable: false, Type: pkg.Go}
	goExecutable := module.Module{IsExecutable: true, Type: pkg.Go}
	unfiltered := []module.Module{node, nodeWindows, temp, goNotExecutable, goExecutable}

	filtered := initc.FilterSuspiciousModules(unfiltered)
	assert.NotContains(t, filtered, node)
	assert.NotContains(t, filtered, nodeWindows)
	assert.NotContains(t, filtered, goNotExecutable)
	assert.NotContains(t, filtered, temp)
	assert.Contains(t, filtered, goExecutable)
}
