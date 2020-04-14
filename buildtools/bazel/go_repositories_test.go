package bazel_test

import (
	"testing"

	"github.com/fossas/fossa-cli/buildtools/bazel"
	"github.com/fossas/fossa-cli/testing/helpers"
	"github.com/stretchr/testify/assert"
)

func TestFileDependencies(t *testing.T) {
	deps, err := bazel.GoBazelDependencies("testdata/repos.bzl")
	assert.Nil(t, err)
	assert.Equal(t, 2, len(deps.Direct))
	helpers.AssertPackageImport(t, deps.Direct, "github.com/test/one", "v1.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "github.com/test/two", "v2.0.0")

	packageOne := helpers.PackageInTransitiveGraph(deps.Transitive, "github.com/test/one", "v1.0.0")
	assert.NotEmpty(t, packageOne)
	assert.Empty(t, packageOne.Imports)

	packageTwo := helpers.PackageInTransitiveGraph(deps.Transitive, "github.com/test/two", "v2.0.0")
	assert.NotEmpty(t, packageTwo)
	assert.Empty(t, packageTwo.Imports)
}
