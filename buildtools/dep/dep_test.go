package dep_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestResolve(t *testing.T) {
	resolver, err := dep.New("testdata/ignore.lock", "testdata/Ignore.toml")
	assert.Equal(t, err, nil)

	// Test that cat/house is not included in the manifest.
	revision, err := resolver.Resolve("cat/house")
	assert.Equal(t, err, buildtools.ErrNoRevisionForPackage)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/puma is an ignored revision.
	revision, err = resolver.Resolve("cat/puma")
	assert.Equal(t, err, buildtools.ErrPackageIsIgnored)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/big/lion is an ignored revision under wildcard rules.
	revision, err = resolver.Resolve("cat/big/lion")
	assert.Equal(t, err, buildtools.ErrPackageIsIgnored)
	assert.Equal(t, revision, pkg.Import{})

	// Test that cat/fossa is an accepted revision.
	revision, err = resolver.Resolve("cat/fossa")
	assert.Equal(t, err, nil)
	assert.Equal(t, revision.Target, "v0.3.0")
}

func TestLockfileGraph(t *testing.T) {
	depGraph, err := dep.LockfileGraph("testdata/Gopkg.lock")
	assert.NoError(t, err)

	assert.Len(t, depGraph.Direct, 3)
	helpers.AssertPackageImport(t, depGraph.Direct, "repo/name/A", "v1.0.0")
	helpers.AssertPackageImport(t, depGraph.Direct, "repo/name/B", "v2.0.0")
	helpers.AssertPackageImport(t, depGraph.Direct, "repo/name/C", "12345")

	assert.Len(t, depGraph.Transitive, 3)
	packageA := helpers.PackageInTransitiveGraph(depGraph.Transitive, "repo/name/A", "v1.0.0")
	assert.NotEmpty(t, packageA)
	assert.Empty(t, packageA.Imports)

	packageB := helpers.PackageInTransitiveGraph(depGraph.Transitive, "repo/name/B", "v2.0.0")
	assert.NotEmpty(t, packageB)
	assert.Empty(t, packageB.Imports)

	packageC := helpers.PackageInTransitiveGraph(depGraph.Transitive, "repo/name/C", "12345")
	assert.NotEmpty(t, packageC)
	assert.Empty(t, packageC.Imports)
}
