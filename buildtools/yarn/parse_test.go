package yarn_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/yarn"
	"github.com/fossas/fossa-cli/pkg"
)

/*
└─┬ chai@4.1.2
  ├── assertion-error@1.1.0
  ├── check-error@1.0.2
  ├─┬ deep-eql@3.0.1
  │ └── type-detect@4.0.8 deduped
  ├── get-func-name@2.0.0
  ├── pathval@1.1.0
  └── type-detect@4.0.8
*/
func TestSimpleLockfile(t *testing.T) {
	deps, err := yarn.FromProject(filepath.Join("testdata", "package.json"), filepath.Join("testdata", "yarn.lock"))
	assert.NoError(t, err)

	assert.Len(t, deps.Direct, 2)
	assert.Len(t, deps.Transitive, 8)

	var chaiImport pkg.Import
	var typeDetectImport pkg.Import

	if deps.Direct[0].Target == "chai" {
		chaiImport = deps.Direct[0]
		typeDetectImport = deps.Direct[1]
	} else {
		chaiImport = deps.Direct[1]
		typeDetectImport = deps.Direct[0]
	}

	assert.Equal(t, "chai", chaiImport.Target)
	assert.Equal(t, "4.1.2", chaiImport.Resolved.Revision)
	assert.Equal(t, "type-detect", typeDetectImport.Target)
	assert.Equal(t, "3.0.0", typeDetectImport.Resolved.Revision)

	AssertDeps(t, deps.Transitive, "assertion-error", "1.1.0")
	AssertDeps(t, deps.Transitive, "check-error", "1.0.2")
	AssertDeps(t, deps.Transitive, "deep-eql", "3.0.1")
	AssertDeps(t, deps.Transitive, "get-func-name", "2.0.0")
	AssertDeps(t, deps.Transitive, "pathval", "1.1.0")
	AssertDeps(t, deps.Transitive, "type-detect", "4.0.8")
	AssertDeps(t, deps.Transitive, "type-detect", "3.0.0")
	AssertDeps(t, deps.Transitive, "chai", "4.1.2")
}

func TestTransitiveCollisionsWithDirectProdDeps(t *testing.T) {
	t.Parallel()
	for _, testNameDirName := range []string{"trans_dep_name_and_revision_collision_with_prod_direct_dep", "trans_dep_name_collision_with_prod_direct_dep"} {
		testPathBase := filepath.Join("testdata", testNameDirName)
		t.Run(testNameDirName, func(t *testing.T) {
			t.Parallel()
			deps, err := yarn.FromProject(filepath.Join(testPathBase, "package.json"), filepath.Join(testPathBase, "yarn.lock"))
			assert.NoError(t, err)

			assert.Len(t, deps.Direct, 1)
			assert.Len(t, deps.Transitive, 1)

			assert.Equal(t, "a", deps.Direct[0].Target)
			assert.Equal(t, "1.0.1", deps.Direct[0].Resolved.Revision)

			AssertDeps(t, deps.Transitive, "a", "1.0.1")
		})
	}
}

func AssertDeps(t *testing.T, transitiveDeps map[pkg.ID]pkg.Package, name, revision string) {
	for pkgID, _ := range transitiveDeps {
		if pkgID.Name == name && pkgID.Revision == revision {
			return
		}
	}

	assert.Fail(t, "missing "+name+"@"+revision)
}
