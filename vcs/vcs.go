// Package vcs implements functions for interacting with version control
// systems.
package vcs

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli"
	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

// Errors that occur when finding VCS repositories.
var (
	ErrNoVCSInDir   = errors.New("could not find VCS repository in directory")
	ErrNoNearestVCS = errors.New("could not find nearest VCS repository in directory")
)

// VCSIn returns the type of VCS repository rooted at a directory, or
// ErrNoVCSInDir if none is found.
func vcsIn(dirname string) (cli.VCS, error) {
	either := monad.EitherVCS{}
	result := either.
		BindVCS(findVCSFolder(cli.Git, filepath.Join(dirname, ".git"))).
		BindVCS(findVCSFolder(cli.Subversion, filepath.Join(dirname, ".svn"))).
		BindVCS(findVCSFolder(cli.Mercurial, filepath.Join(dirname, ".hg"))).
		BindVCS(findVCSFolder(cli.Bazaar, filepath.Join(dirname, ".bzr")))
	if result.Err != nil {
		return 0, result.Err
	}
	if result.Result == 0 {
		return 0, ErrNoVCSInDir
	}
	return result.Result, nil
}

// NearestVCS returns the type and directory of the nearest VCS repository
// containing the directory, or ErrNoNearestVCS if none is found.
func NearestVCS(dirname string) (vcsType cli.VCS, vcsDir string, err error) {
	vcsDir, err = files.WalkUp(dirname, func(d string) error {
		tool, err := vcsIn(d)
		if err == ErrNoVCSInDir {
			return nil
		}
		if err != nil {
			return err
		}
		vcsType = tool
		return files.ErrStopWalk
	})
	if err == files.ErrDirNotFound {
		return 0, "", ErrNoNearestVCS
	}
	return vcsType, vcsDir, err
}

// GetRepository returns the location of the repository containing dirname,
// errutil.ErrRepositoryNotFound if none is found, or errutil.ErrNotImplemented
// if an unsupported VCS is found.
func GetRepository(dirname string) (string, error) {
	vcsType, vcsDir, err := NearestVCS(dirname)
	if err == ErrNoNearestVCS {
		return "", errutil.ErrRepositoryNotFound
	}
	if err != nil {
		return "", err
	}
	switch vcsType {
	case cli.Git:
		return vcsDir, nil
	case cli.Subversion:
		return "", errutil.ErrNotImplemented
	case cli.Mercurial:
		return "", errutil.ErrNotImplemented
	case cli.Bazaar:
		return "", errutil.ErrNotImplemented
	default:
		return "", errutil.ErrNotImplemented
	}
}
