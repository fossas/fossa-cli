// Package vcs implements functions for interacting with version control
// systems.
package vcs

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
)

// Errors that occur when finding VCS repositories.
var (
	ErrNoVCSInDir   = errors.New("could not find VCS repository in directory")
	ErrNoNearestVCS = errors.New("could not find nearest VCS repository in directory")
)

// In returns the type of VCS repository rooted at a directory, or
// ErrNoVCSInDir if none is found.
func In(dirname string) (VCS, error) {
	for _, vcs := range Types {
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

// Nearest returns the type and directory of the nearest VCS repository
// containing the directory, or ErrNoNearestVCS if none is found.
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

// GetRepository returns the location of the repository containing dirname,
// ErrNoNearestVCS if none is found, or errutil.ErrNotImplemented if an
// unsupported VCS is found.
func GetRepository(dirname string) (string, error) {
	vcs, dir, err := Nearest(dirname)
	if err != nil {
		return "", err
	}
	switch vcs {
	case Git:
		return dir, nil
	case Subversion:
		return "", errutil.ErrNotImplemented
	case Mercurial:
		return "", errutil.ErrNotImplemented
	case Bazaar:
		return "", errutil.ErrNotImplemented
	default:
		return "", errutil.ErrNotImplemented
	}
}
