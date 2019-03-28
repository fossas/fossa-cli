package ant_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/ant"
	"github.com/fossas/fossa-cli/pkg"
)

func TestAntDeps(t *testing.T) {
	graph, err := ant.Graph("testdata")
	assert.NoError(t, err)
	assert.Len(t, graph.Direct, 1)
	assertImport(t, graph.Direct, "test")

	assert.Len(t, graph.Transitive, 1)
	packageOne, err := findPackage(graph.Transitive, "test")
	assert.NoError(t, err)
	assert.Len(t, packageOne.Imports, 0)
}

func findPackage(packages map[pkg.ID]pkg.Package, name string) (pkg.Package, error) {
	for id := range packages {
		if id.Name == name {
			return packages[id], nil
		}
	}
	return pkg.Package{}, fmt.Errorf("Package %s not found", name)
}

func assertImport(t *testing.T, imports pkg.Imports, name string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			return
		}
	}
	assert.Fail(t, "missing "+name)
}
