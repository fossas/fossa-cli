package vcs

import (
	"github.com/fossas/fossa-cli"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

// vcsFinder, bindVCSFinder, and friends are EitherVCSType functions for finding whether
// one of many files exist.
type vcsFinder func(pathElems ...string) (bool, error)

func bindVCSFinder(tool cli.VCSType, find vcsFinder, pathElems ...string) monad.EitherVCSTypeFunc {
	return func(prev cli.VCSType) (cli.VCSType, error) {
		ok, err := find(pathElems...)
		if err != nil {
			return 0, err
		}
		if ok {
			return tool, nil
		}
		return prev, nil
	}
}

func findVCSFolder(tool cli.VCSType, pathElems ...string) monad.EitherVCSTypeFunc {
	return bindVCSFinder(tool, files.ExistsFolder, pathElems...)
}
