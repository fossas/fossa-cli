package bower

import "github.com/fossas/fossa-cli/errutil"

type PackageMetadata struct {
	Name    string
	Version string

	TargetName    string `json:"_originalSource"`
	TargetVersion string `json:"_target"`
}

type Package struct {
	PkgMeta      PackageMetadata
	Dependencies map[string]Package
}

type Bower struct {
	Cmd    string
	Config Config
}

func (b *Bower) List(dir string) ([]Package, error) {
	return nil, errutil.ErrNotImplemented
}

func (b *Bower) Clean(dir string) error {
	return errutil.ErrNotImplemented
}

func (b *Bower) Install(dir string, production bool) error {
	return errutil.ErrNotImplemented
}
