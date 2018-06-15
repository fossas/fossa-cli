package golang

import (
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/pkg"
)

// Dir returns the absolute path to a Go package.
func (a *Analyzer) Dir(importpath string) (string, error) {
	pkg, err := a.Go.ListOne(importpath)
	if err != nil {
		return "", err
	}
	return pkg.Dir, nil
}

// Unvendor takes a vendorized import path and strips all vendor folder
// prefixes.
func Unvendor(importpath string) string {
	sections := strings.Split(importpath, "/vendor/")
	return sections[len(sections)-1]
}

// VendorParent returns the directory that contains a vendored directory.
func VendorParent(dirname string) string {
	separator := filepath.FromSlash("/vendor/")
	sections := strings.Split(dirname, separator)
	return strings.Join(sections[:len(sections)-1], separator)
}

// UnresolvedImport returns the default (non-zero) pkg.Import for an unresolved
// gocmd.Package.
func UnresolvedImport(gopkg gocmd.Package) pkg.Import {
	return pkg.Import{
		Target: Unvendor(gopkg.ImportPath),
		Resolved: pkg.ID{
			Type:     pkg.Go,
			Name:     gopkg.ImportPath,
			Revision: "",
			Location: "",
		},
	}
}
