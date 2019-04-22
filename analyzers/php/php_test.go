package php_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/analyzers/php"
)

func TestDiscover(t *testing.T) {
	if testing.Short() {
		t.Skip("Composer requires build tool")
	}

	module, err := php.Discover("testdata", nil)
	assert.NoError(t, err)

	a, err := analyzers.New(module[0])
	assert.NoError(t, err)
	assert.IsType(t, &php.Analyzer{}, a)
}
