package gdm

import (
	"errors"

	"github.com/fossas/fossa-cli/buildtools/gpm"
	"github.com/fossas/fossa-cli/files"
)

// New constructs a gdm lockfile.
func New(dirname string) (gpm.Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, errors.New("directory does not use gdm")
	}

	lockfile, err := gpm.New(dirname, "Godeps")
	if err != nil {
		return nil, err
	}

	return lockfile, nil
}

// UsedIn checks whether gdm is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "Godeps")
}
