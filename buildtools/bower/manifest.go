package bower

import (
	"fmt"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

type Manifest struct {
	Name            string
	DevDependencies map[string]string
	Dependencies    map[string]string
}

func ReadManifest(filename string) (Manifest, *errors.Error) {
	var manifest Manifest
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return Manifest{}, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The bower manifest file `%s` was unable to be read. Check to see if the file exists, can be opened, and is properly formatted JSON", filename),
			Link:            "https://bower.io/docs/creating-packages",
		}
	}
	return manifest, nil
}

func FromComponents(dir string) ([]Package, *errors.Error) {
	return nil, errors.NotImplementedError()
}
