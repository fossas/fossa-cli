package golang_test

import (
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/stretchr/testify/assert"
)

func TestDiscoverUsesRelativePaths(t *testing.T) {
	modules, err := golang.Discover(filepath.Join("..", ".."), nil)
	assert.NoError(t, err)

	t.Logf("%#v", modules)

	for _, m := range modules {
		assert.True(t, !filepath.IsAbs(m.Dir), "%#v is an absolute directory", m.Dir)
	}
}
