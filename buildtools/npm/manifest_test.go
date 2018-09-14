package npm_test

import (
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/pkg"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/stretchr/testify/assert"
)

func TestFromManifest(t *testing.T) {
	pkg, err := npm.PackageFromManifest("fixtures/nested_node_modules/package.json")
	assert.NoError(t, err)

	assert.NotEmpty(t, pkg.Imports)
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

	depGraph, err := npm.FromNodeModules(filepath.Join("fixtures", fixture))
	assert.NoError(t, err)

	assert.Len(t, depGraph.Direct, 2)
	chaiDirectDep := pkg.Import{
		Target: "chai",
		Resolved: pkg.ID{
			Location: "",
			Name:     "chai",
			Revision: "4.1.2",
			Type:     pkg.NodeJS,
		},
	}

	typeDetectDirectDep := pkg.Import{
		Target: "type-detect",
		Resolved: pkg.ID{
			Location: "",
			Name:     "type-detect",
			Revision: "3.0.0",
			Type:     pkg.NodeJS,
		},
	}

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
	assert.True(t, ContainsImport(chaiProject.Imports, "assertion-error", "1.1.0"), "missing assertion-error@1.1.0")
	assert.True(t, ContainsImport(chaiProject.Imports, "check-error", "1.0.2"), "missing check-error@1.0.2")
	assert.True(t, ContainsImport(chaiProject.Imports, "get-func-name", "2.0.0"), "missing get-func-name")
	assert.True(t, ContainsImport(chaiProject.Imports, "pathval", "1.1.0"), "missing pathvalr@1.1.0")
	assert.True(t, ContainsImport(chaiProject.Imports, "type-detect", "4.0.8"), "missing type-detect@4.0.8")
}

func ContainsImport(imports pkg.Imports, packageName string, revision string) bool {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == packageName && importedProj.Resolved.Revision == revision {
			return true
		}
	}

	return false
}
