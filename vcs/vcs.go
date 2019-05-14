// Package vcs supports interacting with version control systems.
package vcs

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

// A System represents the current state of a version-controlled repository.
type System interface {
	// Project returns the string uniquely identifying this codebase.
	Project() string

	// Head returns the latest revision that is being examined.
	Head() Revision
}

type Revision struct {
	Branch     string
	RevisionID string
}

// Errors that occur when finding VCS repositories.
var (
	ErrNoVCSInDir   = errors.New("could not find VCS repository in directory")
	ErrNoNearestVCS = errors.New("could not find nearest VCS repository in directory")
)

// In returns the type of VCS repository rooted at a directory, or
// ErrNoVCSInDir if none is found.
func In(dirname string) (VCS, error) {
	for _, vcs := range findableTypes {
		ok, err := files.ExistsFolder(filepath.Join(dirname, MetadataFolder(vcs)))
		if err != nil {
			return 0, err
		}
		if ok {
			return vcs, nil
		}
	}
	return 0, ErrNoVCSInDir
}

// Nearest returns the type and directory of the nearest VCS repository containing or at
// dirname, or ErrNoNearestVCS if none is found.
func Nearest(dirname string) (VCS, string, error) {
	var vcs VCS
	dir, err := files.WalkUp(dirname, func(d string) error {
		tool, err := In(d)
		if err == ErrNoVCSInDir {
			return nil
		}
		if err != nil {
			return err
		}
		vcs = tool
		return files.ErrStopWalk
	})
	if err == files.ErrDirNotFound {
		return 0, "", ErrNoNearestVCS
	}
	return vcs, dir, err
}
