package dotnet_test

import (
	"testing"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/stretchr/testify/assert"
)

func TestPackagesConfig(t *testing.T) {
	testGraph, err := dotnet.PackageConfigGraph("testdata/packages.config")
	assert.NoError(t, err)

	assert.Len(t, testGraph.Direct, 2)
	assertImport(t, testGraph.Direct, "one", "1.0.0")
	assertImport(t, testGraph.Direct, "two", "2.0.0")

	assert.Len(t, testGraph.Transitive, 2)
	depOne := findPackage(testGraph.Transitive, "one", "1.0.0")
	assert.NotEmpty(t, depOne)
	assert.Empty(t, depOne.Imports)
	depTwo := findPackage(testGraph.Transitive, "two", "2.0.0")
	assert.NotEmpty(t, depTwo)
	assert.Empty(t, depTwo.Imports)
}
