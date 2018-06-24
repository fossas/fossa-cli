package bower

import "github.com/fossas/fossa-cli/errutil"

type Import struct {
	Name    string
	Version string
}

func FromManifest(filename string) ([]Import, error) {
	return nil, errutil.ErrNotImplemented
}

func FromComponents(dir string) ([]Package, error) {
	return nil, errutil.ErrNotImplemented
}
