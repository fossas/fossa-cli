package vcs_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/vcs"
)

func TestNearestAtRoot(t *testing.T) {
	wd, _ := os.Getwd()
	v, dir, err := vcs.Nearest("..")
	assert.NoError(t, err)
	assert.Equal(t, vcs.Git, v)
	assert.Equal(t, filepath.Join(wd, ".."), dir)
}

func TestNearestBelowRoot(t *testing.T) {
	wd, _ := os.Getwd()
	v, dir, err := vcs.Nearest("testdata")
	assert.NoError(t, err)
	assert.Equal(t, vcs.Git, v)
	assert.Equal(t, filepath.Join(wd, ".."), dir)
}

func TestNearestSubversion(t *testing.T) {
	v, dir, err := vcs.Nearest(filepath.Join("testdata", "subversion", "nested", "directory"))
	assert.NoError(t, err)
	assert.Equal(t, vcs.Subversion, v)
	assert.NotEmpty(t, dir)
}

func TestNearestMercurial(t *testing.T) {
	v, dir, err := vcs.Nearest(filepath.Join("testdata", "mercurial"))
	assert.NoError(t, err)
	assert.Equal(t, vcs.Mercurial, v)
	assert.NotEmpty(t, dir)
}

func TestNearestWithoutVCSReturnsErrNoNearestVCS(t *testing.T) {
	_, _, err := vcs.Nearest("/")
	assert.Equal(t, err, vcs.ErrNoNearestVCS)
}
