package vcs

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewMercurialRepository(t *testing.T) {
	if testing.Short() {
		t.Skip("Skip integration test")
	}

	dir := filepath.Join("testdata", "mercurial")
	repo, err := NewMercurialRepository(dir)
	assert.NoError(t, err)

	assert.Equal(t, "https://hg.fossa.com/sample-project", repo.Project())

	head := repo.Head()
	assert.Equal(t, "default", head.Branch)
	assert.Equal(t, "8fb2a764a974c571ab1d7d30482ad99d1342d375", head.RevisionID)

	dirNoRemote := filepath.Join("testdata", "mercurial-noremote")
	repoNoRemote, err := NewMercurialRepository(dirNoRemote)
	assert.NoError(t, err)

	assert.Equal(t, "mercurial-noremote", repoNoRemote.Project())
}
