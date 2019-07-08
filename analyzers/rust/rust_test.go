package rust_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/rust"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Check that we can find Cargo.lock but don't find Cargo.toml.
func TestDiscovery(t *testing.T) {
	options := make(map[string]interface{})
	modules, err := rust.Discover("testdata", options)
	assert.NoError(t, err)
	assert.Len(t, modules, 1)
	expected := module.Module{
		Name:        "testdata/Cargo.lock",
		Type:        pkg.Rust,
		Dir:         ".",
		BuildTarget: ".",
	}
	assert.Equal(t, modules[0], expected)
}
