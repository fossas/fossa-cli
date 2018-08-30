package vcs_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/vcs"
)

func TestGetRepositoryAtRoot(t *testing.T) {
	wd, _ := os.Getwd()
	dir, err := vcs.GetRepository("..")
	assert.NoError(t, err)
	assert.Equal(t, filepath.Join(wd, ".."), dir)
}

func TestGetRepositoryBelowRoot(t *testing.T) {
	wd, _ := os.Getwd()
	dir, err := vcs.GetRepository("testdata")
	assert.NoError(t, err)
	assert.Equal(t, filepath.Join(wd, ".."), dir)
}

func TestGetRepositoryUnsupportedVCS(t *testing.T) {
	dir, err := vcs.GetRepository(filepath.Join("testdata", "subversion", "nested", "directory"))
	assert.Equal(t, err, vcs.ErrUnsupportedVCS)
	assert.Equal(t, "", dir)
}

func TestGetRepositoryWithoutVCSReturnsErrNoNearestVCS(t *testing.T) {
	_, err := vcs.GetRepository("/")
	assert.Equal(t, err, vcs.ErrNoNearestVCS)
}
