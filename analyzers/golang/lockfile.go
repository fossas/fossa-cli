package golang

import (
	"path/filepath"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/files"
)

// Errors that occur while finding lockfiles.
var (
	ErrNoLockfileInDir   = errors.New("could not find lockfile in directory")
	ErrNoNearestLockfile = errors.New("could not find nearest lockfile of directory")
)

// LockfileIn returns the type of lockfile within a directory, or
// ErrNoLockfileInDir if none is found.
func LockfileIn(dirname string) (resolver.Type, error) {
	log.Logger.Debugf("%#v", dirname)

	lockfiles := [][2]string{
		[2]string{"godep", filepath.Join(dirname, "Godeps", "Godeps.json")},
		[2]string{"govendor", filepath.Join(dirname, "vendor", "vendor.json")},
		[2]string{"dep", filepath.Join(dirname, "Gopkg.toml")},
		[2]string{"vndr", filepath.Join(dirname, "vendor.conf")},
		[2]string{"glide", filepath.Join(dirname, "glide.yaml")},
		[2]string{"gdm", filepath.Join(dirname, "Godeps")},
	}

	for _, lockfile := range lockfiles {
		ok, err := files.Exists(lockfile[1])
		if err != nil {
			return "", err
		}
		if ok {
			return resolver.Type(lockfile[0]), nil
		}
	}
	return "", ErrNoLockfileInDir
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
