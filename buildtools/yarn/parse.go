package yarn

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/files"
)

type lockfileEntry struct {
	Version      string
	Dependencies map[string]string
}

type YarnLockfile []lockfileEntry

func FromLockfile(pathElems ...string) (YarnLockfile, error) {
	var lockfile YarnLockfile

	filePath := filepath.Join(pathElems...)

	err := files.ReadYAML(&lockfile, filePath)
	if err != nil {
		return YarnLockfile{}, err
	}

	return lockfile, nil
}
