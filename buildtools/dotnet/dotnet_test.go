package dotnet_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
)

func TestResolveStrategy(t *testing.T) {
	testGraph, err := dotnet.ResolveStrategy("testdata/package-reference/root/test.csproj", "testdata/package-reference/root")
	assert.NoError(t, err)

	assert.Len(t, testGraph.Direct, 2)
	assertImport(t, testGraph.Direct, "one", "1.0.0")
	assertImport(t, testGraph.Direct, "two", "2.0.0")

	assert.Len(t, testGraph.Transitive, 3)
	depOne := findPackage(testGraph.Transitive, "one", "1.0.0")
	assert.NotEmpty(t, depOne)
	assert.Len(t, depOne.Imports, 1)
	assertImport(t, depOne.Imports, "three", "3.0.0")

	depTwo := findPackage(testGraph.Transitive, "two", "2.0.0")
	assert.NotEmpty(t, depTwo)
	assert.Empty(t, depTwo.Imports)
	depThree := findPackage(testGraph.Transitive, "three", "3.0.0")
	assert.NotEmpty(t, depThree)
	assert.Empty(t, depThree.Imports)
}
