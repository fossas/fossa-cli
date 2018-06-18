package vndr

import (
	"errors"

	"github.com/fossas/fossa-cli/buildtools/gpm"
	"github.com/fossas/fossa-cli/files"
)

// New constructs a vndr lockfile.
func New(dirname string) (gpm.Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, errors.New("directory does not use vndr")
	}

	lockfile, err := gpm.FromFile(dirname, "vendor.conf")
	if err != nil {
		return nil, err
	}

	return lockfile, nil
}

// UsedIn checks whether vndr is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "vendor.conf")
}

func FromFile(filename string) (gpm.Lockfile, error) {
	return gpm.FromFile(filename)
}
