package bazel_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/bazel"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestPythonDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/python/BUILD")
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Python, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestRustDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/rust/BUILD.bazel")
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Rust, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestNPMDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/npm/BUILD")
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.NodeJS, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestGoDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/go/BUILD")
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Go, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestCPlusPlusDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/c++/BUILD")
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "c++", "c96385546c3e60884a729ce36f2bc33a")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "c++", "c96385546c3e60884a729ce36f2bc33a")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Raw, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}
