// Package vcs implements functions for interacting with version control
// systems.
package vcs

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

// Errors that occur when finding VCS repositories.
var (
	ErrNoVCSInDir     = errors.New("could not find VCS repository in directory")
	ErrNoNearestVCS   = errors.New("could not find nearest VCS repository in directory")
	ErrUnsupportedVCS = errors.New("VCS type is not supported")
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
		if tool == Subversion || tool == Mercurial || tool == Bazaar {
			return ErrUnsupportedVCS
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
// ErrNoNearestVCS if none is found, or errors.ErrNotImplemented if an
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
		return "", errors.ErrNotImplemented
	case Mercurial:
		return "", errors.ErrNotImplemented
	case Bazaar:
		return "", errors.ErrNotImplemented
	default:
		return "", errors.ErrNotImplemented
	}
}
