// Package vcs implements functions for interacting with version control
// systems.
package vcs

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

type Type int

const (
	_ Type = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

// Errors that occur when finding VCS repositories.
var (
	ErrNoVCSInDir   = errors.New("could not find VCS repository in directory")
	ErrNoNearestVCS = errors.New("could not find nearest VCS repository in directory")
)

// VCSIn returns the type of VCS repository rooted at a directory, or
// ErrNoVCSInDir if none is found.
func VCSIn(dirname string) (string, error) {
	either := monad.EitherStr{}
	result := either.
		Bind(files.FindFolder("git", filepath.Join(dirname, ".git"))).
		Bind(files.FindFolder("svn", filepath.Join(dirname, ".svn"))).
		Bind(files.FindFolder("hg", filepath.Join(dirname, ".hg"))).
		Bind(files.FindFolder("bzr", filepath.Join(dirname, ".bzr")))
	if result.Err != nil {
		return "", result.Err
	}
	if result.Result == "" {
		return "", ErrNoVCSInDir
	}
	return result.Result, nil
}

// NearestVCS returns the type and directory of the nearest VCS repository
// containing the directory, or ErrNoNearestVCS if none is found.
func NearestVCS(dirname string) (vcsName string, vcsDir string, err error) {
	vcsDir, err = files.WalkUp(dirname, func(d string) error {
		tool, err := VCSIn(d)
		if err == ErrNoVCSInDir {
			return nil
		}
		if err != nil {
			return err
		}
		vcsName = tool
		return files.ErrStopWalk
	})
	if err == files.ErrDirNotFound {
		return "", "", ErrNoNearestVCS
	}
	return vcsName, vcsDir, err
}

// GetRepository returns the location of the repository containing dirname,
// errutil.ErrRepositoryNotFound on error.
func GetRepository(dirname string) (string, error) {
	vcsName, vcsDir, err := NearestVCS(dirname)
	if err == ErrNoNearestVCS {
		return "", errutil.ErrRepositoryNotFound
	}
	if err != nil {
		return "", err
	}
	switch vcsName {
	case "git":
		return vcsDir, nil
	case "svn":
		return "", errutil.ErrNotImplemented
	case "hg":
		return "", errutil.ErrNotImplemented
	case "bzr":
		return "", errutil.ErrNotImplemented
	default:
		return "", errutil.ErrNotImplemented
	}
}
