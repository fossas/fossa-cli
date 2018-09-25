package yarn_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/yarn"
	"github.com/fossas/fossa-cli/pkg"
)

func TestParse(t *testing.T) {
	lockfile, err := yarn.FromLockfile("testdata", "yarn.lock")
	assert.NoError(t, err)
	assert.NotEmpty(t, lockfile)
}

var chaiDirectDep = pkg.Import{
	Target: "chai",
	Resolved: pkg.ID{
		Name:     "chai",
		Revision: "4.1.2",
		Type:     pkg.NodeJS,
	},
}

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
	deps, err := yarn.FromLockfile("testdata", "yarn.lock")
	assert.NoError(t, err)

	assert.Len(t, deps.Direct, 1)
	assert.Len(t, deps.Transitive, 7)

	assert.Equal(t, "chai", deps.Direct[0].Target)
	assert.Equal(t, "4.1.2", deps.Direct[0].Resolved.Revision)

	AssertDeps(t, deps.Transitive, "assertion-error", "1.1.0")
	AssertDeps(t, deps.Transitive, "check-error", "1.0.2")
	AssertDeps(t, deps.Transitive, "deep-eql", "3.0.1")
	AssertDeps(t, deps.Transitive, "get-func-name", "2.0.0")
	AssertDeps(t, deps.Transitive, "pathval", "1.1.0")
	AssertDeps(t, deps.Transitive, "type-detect", "4.0.8")
	AssertDeps(t, deps.Transitive, "chai", "4.1.2")
}

func AssertDeps(t *testing.T, transitiveDeps map[pkg.ID]pkg.Package, name, revision string) {
	for pkgID, _ := range transitiveDeps {
		if pkgID.Name == name && pkgID.Revision == revision {
			return
		}
	}

	assert.Fail(t, "missing "+name+"@"+revision)
}
