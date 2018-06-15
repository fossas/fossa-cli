package golang

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

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
	return bindFinder(tool, files.Exists, pathElems...)
}

func findFolder(tool string, pathElems ...string) monad.EitherStrFunc {
	return bindFinder(tool, files.ExistsFolder, pathElems...)
}
