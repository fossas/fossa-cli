package npm_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/pkg"
)

func getDirectDepImports() (chaiDirectDep pkg.Import, typeDetectDirectDep pkg.Import) {
	chaiDirectDep = pkg.Import{
		Target: "chai",
		Resolved: pkg.ID{
			Location: "",
			Name:     "chai",
			Revision: "4.1.2",
			Type:     pkg.NodeJS,
		},
	}

	typeDetectDirectDep = pkg.Import{
		Target: "type-detect",
		Resolved: pkg.ID{
			Location: "",
			Name:     "type-detect",
			Revision: "3.0.0",
			Type:     pkg.NodeJS,
		},
	}

	return
}

func TestFromManifest(t *testing.T) {
	pkg, err := npm.PackageFromManifest(filepath.Join("fixtures", "nested_node_modules/"), "package.json")
	assert.NoError(t, err)

	chaiDirectDep, typeDetectDirectDep := getDirectDepImports()

	assert.Len(t, pkg.Imports, 2)
	assert.Contains(t, pkg.Imports, chaiDirectDep)
	assert.Contains(t, pkg.Imports, typeDetectDirectDep)
}

func TestFromNodeModules(t *testing.T) {
	testFromNodeModulesByFixture(t, "flattened_node_modules")
	testFromNodeModulesByFixture(t, "nested_node_modules")
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

func testFromNodeModulesByFixture(t *testing.T, fixture string) {
	chaiDirectDep, typeDetectDirectDep := getDirectDepImports()
	depGraph, err := npm.FromNodeModules(filepath.Join("fixtures", fixture), "package.json")
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

	chaiProject := depGraph.Transitive[chaiDirectDep.Resolved]
	assert.NotNil(t, chaiProject)
	assert.Len(t, chaiProject.Imports, 6)
	AssertImport(t, chaiProject.Imports, "assertion-error", "1.1.0")
	AssertImport(t, chaiProject.Imports, "check-error", "1.0.2")
	AssertImport(t, chaiProject.Imports, "get-func-name", "2.0.0")
	AssertImport(t, chaiProject.Imports, "pathval", "1.1.0")
	AssertImport(t, chaiProject.Imports, "type-detect", "4.0.8")
}

func AssertImport(t *testing.T, imports pkg.Imports, name, revision string) {
	// Inline the `ContainsImport` logic.
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name && importedProj.Resolved.Revision == revision {
			return
		}
	}

	assert.Fail(t, "missing "+name+"@"+revision)
}
