package files

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/monad"
)

// blaf
var (
	ErrDirNotFound = errors.New("no directory found during walk")
	ErrStopWalk    = errors.New("WalkUp: stop")
)

// A WalkUpFunc takes a directory and returns an error.
type WalkUpFunc func(dir string) error

// WalkUp takes a starting directory and a WalkUpFunc, and calls the function,
// passing each ancestor of the starting directory in upwards order until the
// filesystem root is reached.
//
// If the function returns ErrStopWalk, then WalkUp stops and returns the
// current directory name. If the function returns any other error, then WalkUp
// stops and that error is returned as the error of WalkUp. If ErrStopWalk is
// never returned, WalkUp returns ErrDirNotFound.
func WalkUp(startdir string, walker WalkUpFunc) (string, error) {
	dir, err := filepath.Abs(startdir)
	if err != nil {
		return "", err
	}

	for ; dir != "/"; dir = filepath.Dir(dir) {
		err := walker(dir)
		if err == ErrStopWalk {
			return dir, nil
		}
		if err != nil {
			return "", err
		}
	}

	// Run once at the filesystem root.
	err = walker(dir)
	if err == ErrStopWalk {
		return dir, nil
	}
	if err != nil {
		return "", err
	}
	return "", ErrDirNotFound
}

// finder, bindFinder, and friends are EitherStr functions for finding whether
// one of many files exist.
type finder func(pathElems ...string) (bool, error)

func bindFinder(name string, find finder, pathElems ...string) monad.EitherStrFunc {
	return func(prev string) (string, error) {
		ok, err := find(pathElems...)
		if err != nil {
			return "", err
		}
		if ok {
			return name, nil
		}
		return prev, nil
	}
}

func findFile(tool string, pathElems ...string) monad.EitherStrFunc {
	return bindFinder(tool, Exists, pathElems...)
}

func FindFolder(tool string, pathElems ...string) monad.EitherStrFunc {
	return bindFinder(tool, ExistsFolder, pathElems...)
}
