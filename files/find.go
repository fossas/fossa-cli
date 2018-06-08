package files

import (
	"errors"
	"path/filepath"
)

var ErrDirNotFound = errors.New("no directory found during walk")

type WalkPredicate func(dir string) (bool, error)

func WalkUp(startdir string, predicate WalkPredicate) (string, error) {
	dir, err := filepath.Abs(startdir)
	if err != nil {
		return "", err
	}

	for ; dir != "/"; dir = filepath.Dir(dir) {
		found, err := predicate(dir)
		if err != nil {
			return "", err
		}
		if found {
			return dir, nil
		}
	}
	found, err := predicate(dir)
	if err != nil {
		return "", err
	}
	if found {
		return dir, nil
	}
	return "", ErrDirNotFound
}
