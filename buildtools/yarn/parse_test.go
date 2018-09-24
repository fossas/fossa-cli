package yarn_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/yarn"
)

func TestParse(t *testing.T) {
	lockfile, err := yarn.FromLockfile("testdata", "yarn.lock")
	assert.NoError(t, err)
	assert.NotEmpty(t, lockfile)
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

	assert.Len(t, pkg.Imports, 1)
}
