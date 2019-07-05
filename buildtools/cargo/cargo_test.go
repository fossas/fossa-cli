package cargo_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/cargo"
	"github.com/fossas/fossa-cli/testing/helpers"
)

//Test that we can analyze a Cargo.lock and that we find direct deps from other
func TestLockfile(t *testing.T) {
	graph, err := cargo.LockfileDependencies("testdata/Cargo.lock", "testdata")
	assert.NoError(t, err)

	assert.Len(t, graph.Direct, 2)
	assert.Len(t, graph.Transitive, 5)
}

// Cargo toml  test cases
// 1. Deps
//	Different formats? Referencing other manifests?
// 2. Build Deps
// 3. Sub manifests
