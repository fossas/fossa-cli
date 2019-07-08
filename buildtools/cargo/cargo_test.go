package cargo_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/cargo"
	"github.com/fossas/fossa-cli/testing/helpers"
)

// Test that we can analyze a Cargo.lock and that direct deps are found in Cargo.toml files.
func TestLockfileWithManifest(t *testing.T) {
	graph, err := cargo.LockfileDependencies("Cargo.lock", "testdata")
	assert.NoError(t, err)
	assert.Len(t, graph.Direct, 2)
	assert.Len(t, graph.Transitive, 5)

	helpers.AssertPackageImport(t, graph.Direct, "one", "1.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "two", "2.0.0")

	one := helpers.PackageInTransitiveGraph(graph.Transitive, "one", "1.0.0")
	assert.Len(t, one.Imports, 1)
	helpers.AssertPackageImport(t, one.Imports, "three", "3.0.0")

	two := helpers.PackageInTransitiveGraph(graph.Transitive, "two", "2.0.0")
	assert.Len(t, two.Imports, 2)
	helpers.AssertPackageImport(t, two.Imports, "three", "3.0.0")
	helpers.AssertPackageImport(t, two.Imports, "five", "5.0.0")

	three := helpers.PackageInTransitiveGraph(graph.Transitive, "three", "3.0.0")
	assert.Len(t, three.Imports, 1)
	helpers.AssertPackageImport(t, three.Imports, "four", "4.0.0")

	four := helpers.PackageInTransitiveGraph(graph.Transitive, "four", "4.0.0")
	assert.Len(t, four.Imports, 0)

	five := helpers.PackageInTransitiveGraph(graph.Transitive, "five", "5.0.0")
	assert.Len(t, five.Imports, 0)
}

func TestLockfileNoManifest(t *testing.T) {
	graph, err := cargo.LockfileDependencies("Cargo.lock", "testdata/no-manifests")
	assert.NoError(t, err)
	assert.Len(t, graph.Direct, 0)
	assert.Len(t, graph.Transitive, 1)

	one := helpers.PackageInTransitiveGraph(graph.Transitive, "one", "1.0.0")
	assert.Len(t, one.Imports, 0)
}

func TestMissingManifest(t *testing.T) {
	graph, err := cargo.LockfileDependencies("Cargo.lock", "testdata/missing-manifest")
	assert.NoError(t, err)
	assert.Len(t, graph.Direct, 0)
	assert.Len(t, graph.Transitive, 1)

	one := helpers.PackageInTransitiveGraph(graph.Transitive, "one", "1.0.0")
	assert.Len(t, one.Imports, 0)
}
