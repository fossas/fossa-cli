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
├─ chai@4.1.2
│  ├─ assertion-error@^1.0.1
│  ├─ check-error@^1.0.1
│  ├─ deep-eql@^3.0.0
│  ├─ get-func-name@^2.0.0
│  ├─ pathval@^1.0.0
│  └─ type-detect@^4.0.0
*/
func TestSimpleLockfile(t *testing.T) {
	pkg, err := yarn.FromLockfile("testdata", "yarn.lock")
	assert.NoError(t, err)

	assert.Len(t, pkg.Direct, 1)
	assert.Len(t, pkg.Transitive, 7)
	chaiDep := pkg.Imports[chaiDirectDep]
}
