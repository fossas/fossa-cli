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
