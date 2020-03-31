package npm_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

var chaiDirectDep = pkg.Import{
	Target: "chai",
	Resolved: pkg.ID{
		Location: "",
		Name:     "chai",
		Revision: "4.1.2",
		Type:     pkg.NodeJS,
	},
}

var typeDetectDirectDep = pkg.Import{
	Target: "type-detect",
	Resolved: pkg.ID{
		Location: "",
		Name:     "type-detect",
		Revision: "3.0.0",
		Type:     pkg.NodeJS,
	},
}

func TestFromManifest(t *testing.T) {
	pkg, err := npm.PackageFromManifest(false, filepath.Join("testdata", "nested_node_modules"), "package.json")
	assert.NoError(t, err)

	assert.Len(t, pkg.Imports, 2)
	assert.Contains(t, pkg.Imports, chaiDirectDep)
	assert.Contains(t, pkg.Imports, typeDetectDirectDep)
}

func TestFromNodeModules(t *testing.T) {
	testByFixture(t, npm.FromNodeModules, "flattened_node_modules")
	testByFixture(t, npm.FromNodeModules, "nested_node_modules")
}

func TestFromLockfile(t *testing.T) {
	testByFixture(t, npm.FromLockfile, "only_package_lock")
}

/*
	├─┬ chai@4.1.2
	│ ├── assertion-error@1.1.0
	│ ├── check-error@1.0.2
	│ ├─┬ deep-eql@3.0.1
	│ │ └── type-detect@4.0.8
	│ ├── get-func-name@2.0.0
	│ ├── pathval@1.1.0
	│ └── type-detect@4.0.8
	└── type-detect@3.0.0
*/

func testByFixture(t *testing.T, f func(string, bool) (graph.Deps, error), fixture string) {
	depGraph, err := f(filepath.Join("testdata", fixture), false)
	assert.NoError(t, err)

	assert.Len(t, depGraph.Direct, 2)

	assert.Contains(t, depGraph.Direct, chaiDirectDep)
	assert.Contains(t, depGraph.Direct, typeDetectDirectDep)

	assert.Len(t, depGraph.Transitive, 8)

	typeDetectTransitiveDepKey := pkg.ID{
		Location: "",
		Name:     "type-detect",
		Revision: "4.0.8",
		Type:     pkg.NodeJS,
	}

	// ensure project that is both a transitive and direct dep have correct versions based on where they live
	assert.Contains(t, depGraph.Transitive, typeDetectTransitiveDepKey)
	assert.Contains(t, depGraph.Transitive, typeDetectDirectDep.Resolved)

	chaiProject := depGraph.Transitive[chaiDirectDep.Resolved]
	assert.Equal(t, chaiDirectDep.Resolved, chaiProject.ID)
	assert.NotNil(t, chaiProject)
	assert.Len(t, chaiProject.Imports, 6)
	AssertImport(t, chaiProject.Imports, "assertion-error", "1.1.0")
	AssertImport(t, chaiProject.Imports, "check-error", "1.0.2")
	AssertImport(t, chaiProject.Imports, "get-func-name", "2.0.0")
	AssertImport(t, chaiProject.Imports, "pathval", "1.1.0")
	AssertImport(t, chaiProject.Imports, "type-detect", "4.0.8")

	typeDetectProject := depGraph.Transitive[typeDetectDirectDep.Resolved]
	assert.Equal(t, typeDetectDirectDep.Resolved, typeDetectProject.ID)
	assert.Empty(t, typeDetectProject.Imports)
}

func AssertImport(t *testing.T, imports pkg.Imports, name, revision string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name && importedProj.Resolved.Revision == revision {
			return
		}
	}

	assert.Fail(t, "missing "+name+"@"+revision)
}
