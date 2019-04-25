package vcs

import (
	"path/filepath"
	"strconv"
	"time"

	"github.com/fossas/fossa-cli/errors"
)

// NoRepository implements the System interface. This is used for projects that are not version controlled.
type NoRepository struct {
	dir     string
	project string
	// revisionId is the current time because this is easy on the CI process.
	revisionId time.Time
}

// NewNoRepository uses the name of dir and the current time to identify the codebase.
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

func (nr *NoRepository) Project() string { return nr.project }

func (nr *NoRepository) Head() Revision {
	return Revision{
		Branch:     "none",
		RevisionID: strconv.FormatInt(nr.revisionId.UnixNano(), 16),
	}
}
