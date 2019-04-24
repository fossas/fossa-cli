package dotnet_test

import (
	"fmt"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestProjectJSON(t *testing.T) {
	testGraph, err := dotnet.ProjectGraph("testdata/project.json")
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

func findPackage(packages map[pkg.ID]pkg.Package, name, revision string) pkg.Package {
	for id := range packages {
		if id.Name == name && id.Revision == revision {
			return packages[id]
		}
	}
	return pkg.Package{}
}

func assertImport(t *testing.T, dependencies []pkg.Import, name, version string) {
	for _, dependency := range dependencies {
		if dependency.Resolved.Name == name && dependency.Resolved.Revision == version {
			return
		}
	}
	assert.Fail(t, fmt.Sprintf("Imports list: %+v\nDoes not contain\nDependency: %s", dependencies, name))
}
