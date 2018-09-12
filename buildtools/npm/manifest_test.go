package npm_test

import (
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/pkg"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/stretchr/testify/assert"
)

// func TestFromManifest(t *testing.T) {
// 	manifest, err := npm.FromManifest("testdata/package.json")
// 	assert.NoError(t, err)

// 	assert.NotEmpty(t, manifest.Dependencies)
// 	assert.Equal(t, manifest.Dependencies["chai"], "4.1.2")
// }

func TestFromNodeModules(t *testing.T) {
	// t.Skip("not yet implemented")
	// testFromNodeModulesByFixture(t, "flattened_node_modules")
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
	// t.Skip("not yet implemented")

	depGraph, err := npm.FromNodeModules(filepath.Join("fixtures", fixture))
	assert.NoError(t, err)

	assert.Len(t, depGraph.Transitive, 9)

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
}
