package dotnet_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
)

func TestPackageReference(t *testing.T) {
	// `test.csproj` lists 2 dependencies and references another package reference file
	// `transitive.csproj` which contains 1 additional dependency and is only
	// referenced inside of `test.csproj`.
	testGraph, err := dotnet.PackageReferenceGraph("testdata/package-reference/root/test.csproj")
	assert.NoError(t, err)

	assert.Len(t, testGraph.Direct, 3)
	assertImport(t, testGraph.Direct, "one", "[1.0,2.0)")
	assertImport(t, testGraph.Direct, "two", "2.0.0")
	assertImport(t, testGraph.Direct, "three", "3.0.0")

	assert.Len(t, testGraph.Transitive, 3)
	depOne := findPackage(testGraph.Transitive, "one", "[1.0,2.0)")
	assert.NotEmpty(t, depOne)
	assert.Empty(t, depOne.Imports)
	depTwo := findPackage(testGraph.Transitive, "two", "2.0.0")
	assert.NotEmpty(t, depTwo)
	assert.Empty(t, depTwo.Imports)
	depThree := findPackage(testGraph.Transitive, "three", "3.0.0")
	assert.NotEmpty(t, depThree)
	assert.Empty(t, depThree.Imports)
}

func TestIsPackageReferenceFile(t *testing.T) {
	assert.True(t, dotnet.IsPackageReferenceFile("test.csproj"))
	assert.True(t, dotnet.IsPackageReferenceFile("test.temp.xproj"))
	assert.True(t, dotnet.IsPackageReferenceFile("test.project.temp.vbproj"))
	assert.True(t, dotnet.IsPackageReferenceFile("test.dbproj"))
	assert.True(t, dotnet.IsPackageReferenceFile("test.fsproj"))
	assert.False(t, dotnet.IsPackageReferenceFile("temp.proj"))
	assert.False(t, dotnet.IsPackageReferenceFile("packages.nuspec"))
}
