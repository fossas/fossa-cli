package ant_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/ant"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestAntDeps(t *testing.T) {
	graph, err := ant.Graph("testdata")
	assert.Nil(t, err)
	assert.Len(t, graph.Direct, 1)
	helpers.AssertPackageImport(t, graph.Direct, "test", "1.2.3")

	assert.Len(t, graph.Transitive, 1)
	packageOne := helpers.PackageInTransitiveGraph(graph.Transitive, "test", "1.2.3")
	assert.Len(t, packageOne.Imports, 0)
}
