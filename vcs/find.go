package vcs

import (
	"github.com/fossas/fossa-cli"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

// vcsFinder, bindVCSFinder, and friends are EitherVCS functions for finding whether
// one of many files exist.
type vcsFinder func(pathElems ...string) (bool, error)

func bindVCSFinder(tool cli.VCS, find vcsFinder, pathElems ...string) monad.EitherVCSFunc {
	return func(prev cli.VCS) (cli.VCS, error) {
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

func findVCSFolder(tool cli.VCS, pathElems ...string) monad.EitherVCSFunc {
	return bindVCSFinder(tool, files.ExistsFolder, pathElems...)
}
