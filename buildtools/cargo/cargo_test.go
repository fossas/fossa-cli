package cargo_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/cargo"
)

func TestLockfile(t *testing.T) {
	graph, _ := cargo.LockfileDependencies("testdata/Cargo.lock", "testdata")
	assert.NotEmpty(t, graph)
	fmt.Println(graph.Direct)
}
