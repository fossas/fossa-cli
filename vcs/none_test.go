package vcs_test

import (
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/fossas/fossa-cli/vcs"
	"github.com/stretchr/testify/assert"
)

func TestNewNoRepository(t *testing.T) {
	dir := filepath.Join("testdata", "none")
	repo, err := vcs.NewNoRepository(dir)
	assert.NoError(t, err)

	assert.Equal(t, "none", repo.Project())

	head := repo.Head()
	revision := head.RevisionID
	assert.True(t, len(revision) > 8)
	revisionTime, err := strconv.ParseInt(revision, 16, 63)
	assert.NoError(t, err)
	assert.True(t, revisionTime <= time.Now().UnixNano())
	assert.Equal(t, head.Branch, "none")
}
