package vcs

import (
	"path/filepath"
	"strconv"
	"time"

	"github.com/fossas/fossa-cli/errors"
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
	path, err := filepath.Abs(dir)
	if err != nil {
		return nil, errors.Wrap(err, "could not resolve absolute path to project root")
	}
	return &NoRepository{
		dir:        dir,
		project:    filepath.Base(path),
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
