package vndr

import (
	"errors"

	"github.com/fossas/fossa-cli/buildtools/gpm"
	"github.com/fossas/fossa-cli/files"
)

// New constructs a vndr lockfile.
func New(lockfilePath string) (gpm.Lockfile, error) {
	ok, err := UsedIn(lockfilePath)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, errors.New("directory does not use vndr")
	}

	lockfile, err := gpm.FromFile(lockfilePath)
	if err != nil {
		return nil, err
	}

	return lockfile, nil
}

// UsedIn checks whether vndr is used correctly within a project folder.
func UsedIn(lockfilePath string) (bool, error) {
	return files.Exists(lockfilePath)
}

func FromFile(filename string) (gpm.Lockfile, error) {
	return gpm.FromFile(filename)
}
