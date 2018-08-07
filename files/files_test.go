package files_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/files"
)

func TestNonExistentParentIsNotErr(t *testing.T) {
	ok, err := files.Exists(filepath.Join("testdata", "parent", "does", "not", "exist", "file"))
	assert.NoError(t, err)
	assert.False(t, ok)
}
