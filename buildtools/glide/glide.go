package glide

import (
	"errors"

	"github.com/fossas/fossa-cli/files"
)

// ErrNoLockfile is returned if a dep manifest is found without an accompanying lockfile.
var ErrNoLockfile = errors.New("found Gopkg.toml but not Gopkg.lock")

func UsedIn(dirname string) (bool, error) {
	// Check whether there exists a manifest.
	ok, err := files.Exists(dirname, "glide.yaml")
	if err != nil {
		return false, err
	}

	if ok {
		// Check whether there exists a lockfile.
		ok, err := files.Exists(dirname, "glide.lock")
		if err != nil {
			return false, err
		}
		// If both exist, then dep is being used correctly.
		if ok {
			return true, nil
		}
		// If only a manifest exists, then return ErrNoLockfile.
		return true, ErrNoLockfile
	}
	// If there is no manifest, then dep is not being used.
	return false, nil
}
