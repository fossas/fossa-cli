package vcs

import (
	"path/filepath"
	"strconv"
	"time"
)

// A NoRepository system is used in a project that is not version controlled.
type NoRepository struct {
	dir        string
	project    string
	revisionId time.Time
}

// NewNoRepository takes the directory marking the root of a codebase that is not version controlled and
// returns an implementation of the System interface that uses the filesystem and current time as metadata.
func NewNoRepository(dir string) (*NoRepository, error) {
	return &NoRepository{
		dir:        dir,
		project:    filepath.Base(dir),
		revisionId: time.Now(),
	}, nil
}

func (nr *NoRepository) Project() string {
	return nr.project
}

func (nr *NoRepository) Head() Revision {
	return Revision{
		Branch:     "none",
		RevisionID: strconv.FormatInt(nr.revisionId.UnixNano(), 16),
	}
}
