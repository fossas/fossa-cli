package paket_test

import (
	"fmt"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/paket"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestPaketDependencyGraph(t *testing.T) {
	graph, err := paket.DependencyGraph("testdata/paket.lock")
	assert.NoError(t, err)
	assert.NotEmpty(t, graph)
	assertImport(t, graph.Direct, "Dep.One", "1.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "Dep.Two", "2.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "Dep.Three", "3.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "Dep.Four", "4.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "Dep.Five", "5.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "Dep.Six", "6.0.0", pkg.NuGet)
	assertImport(t, graph.Direct, "user/repo/src/app/file", "0987654321", pkg.Git)
	assertImport(t, graph.Direct, "www/website/src/file", "123456789", pkg.Git)
	assertImport(t, graph.Direct, "user/otherrepo/src/secondfile", "5678901234", pkg.Git)

	dependencyOne, err := findPackage(graph.Transitive, "Dep.One")
	assert.NoError(t, err)
	assert.Empty(t, dependencyOne.Imports)

	dependencyTwo, err := findPackage(graph.Transitive, "Dep.Two")
	assert.NoError(t, err)
	assertImport(t, dependencyTwo.Imports, "Dep.Three", "3.0.0", pkg.NuGet)

	dependencyThree, err := findPackage(graph.Transitive, "Dep.Three")
	assert.NoError(t, err)
	assertImport(t, dependencyThree.Imports, "Dep.Four", "4.0.0", pkg.NuGet)

	dependencyFour, err := findPackage(graph.Transitive, "Dep.Four")
	assert.NoError(t, err)
	assert.Empty(t, dependencyFour.Imports)

	dependencyFive, err := findPackage(graph.Transitive, "Dep.Five")
	assert.NoError(t, err)
	assertImport(t, dependencyFive.Imports, "Dep.Six", "6.0.0", pkg.NuGet)

	dependencySix, err := findPackage(graph.Transitive, "Dep.Six")
	assert.NoError(t, err)
	assert.Empty(t, dependencySix.Imports)

	dependencyGitHub, err := findPackage(graph.Transitive, "user/repo/src/app/file")
	assert.NoError(t, err)
	assert.Empty(t, dependencyGitHub.Imports)

	dependencyHTTP, err := findPackage(graph.Transitive, "www/website/src/file")
	assert.NoError(t, err)
	assert.Empty(t, dependencyHTTP.Imports)

	dependencyGit, err := findPackage(graph.Transitive, "user/otherrepo/src/secondfile")
	assert.NoError(t, err)
	assert.Empty(t, dependencyGit.Imports)
}

func findPackage(packages map[pkg.ID]pkg.Package, name string) (pkg.Package, error) {
	for id := range packages {
		if id.Name == name {
			return packages[id], nil
		}
	}
	return pkg.Package{}, fmt.Errorf("Package %s not found", name)
}

func assertImport(t *testing.T, imports pkg.Imports, name, revision string, packageType pkg.Type) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			if importedProj.Resolved.Revision == revision {
				if importedProj.Resolved.Type == packageType {
					return
				}
			}
		}
	}
	assert.Fail(t, "missing "+name)
}
