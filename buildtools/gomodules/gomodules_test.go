package gomodules_test

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/gomodules"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestResolver(t *testing.T) {
	file, err := ioutil.ReadFile(filepath.Join("testdata", "go-list-all"))
	assert.NoError(t, err)

	resolver, err := gomodules.Mock(string(file))
	assert.NoError(t, err)
	assert.NotEmpty(t, resolver)

	revision, err := resolver.Resolve("test/basic")
	assert.NoError(t, err)
	assert.Equal(t, "test/basic", revision.Resolved.Name)
	assert.Equal(t, "v1.1.1", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/basic/with/extensions")
	assert.NoError(t, err)
	assert.Equal(t, "test/basic", revision.Resolved.Name)
	assert.Equal(t, "v1.1.1", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/basic/monorepo/package")
	assert.NoError(t, err)
	assert.Equal(t, "test/basic/monorepo", revision.Resolved.Name)
	assert.Equal(t, "v2.0.0", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/versionzero")
	assert.NoError(t, err)
	assert.Equal(t, "test/versionzero", revision.Resolved.Name)
	assert.Equal(t, "123456789000", revision.Resolved.Revision)

	revision, err = resolver.Resolve("replace/fork")
	assert.NoError(t, err)
	assert.Equal(t, "private/fork", revision.Resolved.Name)
	assert.Equal(t, "v9.0.0", revision.Resolved.Revision)

	revision, err = resolver.Resolve("replace/version")
	assert.NoError(t, err)
	assert.Equal(t, "replace/version", revision.Resolved.Name)
	assert.Equal(t, "v1.0.0", revision.Resolved.Revision)

	revision, err = resolver.Resolve("test/failure")
	assert.Equal(t, buildtools.ErrNoRevisionForPackage, err)
}

func TestGoModGraph(t *testing.T) {
	depGraph, err := gomodules.ModGraph("testdata/go.mod")
	assert.NoError(t, err)

	assert.Len(t, depGraph.Direct, 5)
	helpers.AssertPackageImport(t, depGraph.Direct, "repo/name/A", "v1.0.0")
	helpers.AssertPackageImport(t, depGraph.Direct, "alias/repo/B", "v0.1.0")
	helpers.AssertPackageImport(t, depGraph.Direct, "alias/repo/C", "000000000003")
	helpers.AssertPackageImport(t, depGraph.Direct, "repo/name/D", "v4.0.0")
	helpers.AssertPackageImport(t, depGraph.Direct, "alias/repo/E", "000000000005")

	assert.Len(t, depGraph.Transitive, 5)
	packageA := helpers.PackageInTransitiveGraph(depGraph.Transitive, "repo/name/A", "v1.0.0")
	assert.NotEmpty(t, packageA)
	assert.Empty(t, packageA.Imports)

	packageB := helpers.PackageInTransitiveGraph(depGraph.Transitive, "alias/repo/B", "v0.1.0")
	assert.NotEmpty(t, packageB)
	assert.Empty(t, packageB.Imports)

	packageC := helpers.PackageInTransitiveGraph(depGraph.Transitive, "alias/repo/C", "000000000003")
	assert.NotEmpty(t, packageC)
	assert.Empty(t, packageC.Imports)

	packageD := helpers.PackageInTransitiveGraph(depGraph.Transitive, "repo/name/D", "v4.0.0")
	assert.NotEmpty(t, packageD)
	assert.Empty(t, packageD.Imports)

	packageE := helpers.PackageInTransitiveGraph(depGraph.Transitive, "alias/repo/E", "000000000005")
	assert.NotEmpty(t, packageE)
	assert.Empty(t, packageE.Imports)
}
