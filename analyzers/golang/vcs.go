package golang

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
	"github.com/pkg/errors"
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
		Bind(findFolder("git", filepath.Join(dirname, ".git"))).
		Bind(findFolder("svn", filepath.Join(dirname, ".svn"))).
		Bind(findFolder("hg", filepath.Join(dirname, ".hg"))).
		Bind(findFolder("bzr", filepath.Join(dirname, ".bzr")))
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
