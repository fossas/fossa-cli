package bower

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

type Manifest struct {
	Name            string
	DevDependencies map[string]string
	Dependencies    map[string]string
}

func ReadManifest(filename string) (Manifest, error) {
	var manifest Manifest
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return Manifest{}, err
	}
	return manifest, nil
}

func FromComponents(dir string) ([]Package, error) {
	return nil, errors.ErrNotImplemented
}
