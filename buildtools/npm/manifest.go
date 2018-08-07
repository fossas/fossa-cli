package npm

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

type Manifest struct {
	Name    string
	Version string

	Dependencies map[string]string
}

func FromManifest(filename string) (Manifest, error) {
	var manifest Manifest
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return Manifest{}, err
	}
	return manifest, nil
}

func FromNodeModules(dir string) ([]Manifest, error) {
	return nil, errors.ErrNotImplemented
}

type Lockfile struct {
	Dependencies map[string]struct {
		Version  string
		Requires map[string]string
	}
}

func FromLockfile(filename string) (Lockfile, error) {
	return Lockfile{}, errors.ErrNotImplemented
}
