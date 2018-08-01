package vcs_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/vcs"
)

func TestGetRepositoryAtRoot(t *testing.T) {
	wd, _ := os.Getwd()
	vcsDir, err := vcs.GetRepository(filepath.Join("testdata", "git"))
	assert.NoError(t, err)
	assert.Equal(t, filepath.Join(wd, "testdata", "git"), vcsDir)
}

func TestGetRepositoryBelowRoot(t *testing.T) {
	wd, _ := os.Getwd()
	vcsDir, err := vcs.GetRepository(filepath.Join("testdata", "git", "nested", "directory"))
	assert.NoError(t, err)
	assert.Equal(t, filepath.Join(wd, "testdata", "git"), vcsDir)
}

func TestGetRepositoryUnsupportedVCS(t *testing.T) {
	vcsDir, err := vcs.GetRepository(filepath.Join("testdata", "subversion", "nested", "directory"))
	assert.Error(t, err, errutil.ErrNotImplemented)
	assert.Equal(t, "", vcsDir)
}
