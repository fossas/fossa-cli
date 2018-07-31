package golang

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/monad"
)

func findFile(tool string, pathElems ...string) monad.EitherStrFunc {
	return files.BindFinder(tool, files.Exists, pathElems...)
}
