package golang

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
	"github.com/pkg/errors"
)

// Errors that occur while finding lockfiles.
var (
	ErrNoLockfileInDir   = errors.New("could not find lockfile in directory")
	ErrNoNearestLockfile = errors.New("could not nearest lockfile of directory")
)

// LockfileIn returns the type of lockfile within a directory, or
// ErrNoLockfileInDir if none is found.
func LockfileIn(dirname string) (resolver.Type, error) {
	either := monad.EitherStr{}
	result := either.
		Bind(findFile("godep", filepath.Join(dirname, "Godeps", "Godeps.json"))).
		Bind(findFile("govendor", filepath.Join(dirname, "vendor", "vendor.json"))).
		Bind(findFile("dep", filepath.Join(dirname, "Gopkg.toml"))).
		Bind(findFile("vndr", filepath.Join(dirname, "vendor.conf"))).
		Bind(findFile("glide", filepath.Join(dirname, "glide.yaml"))).
		Bind(findFile("gdm", filepath.Join(dirname, "Godeps")))
	if result.Err != nil {
		return "", result.Err
	}
	if result.Result == "" {
		return "", ErrNoLockfileInDir
	}
	return resolver.Type(result.Result), nil
}

// NearestLockfile returns the type and directory of the nearest lockfile in an
// ancestor directory, or ErrNoNearestLockfile if none is found.
func NearestLockfile(dirname string) (resolver.Type, string, error) {
	var tool resolver.Type
	manifestDir, err := files.WalkUp(dirname, func(d string) (err error) {
		tool, err = LockfileIn(d)
		if err == ErrNoLockfileInDir {
			return nil
		}
		if err != nil {
			return err
		}
		return files.ErrStopWalk
	})
	if err == files.ErrDirNotFound {
		return "", "", ErrNoNearestLockfile
	}
	return tool, manifestDir, err
}
