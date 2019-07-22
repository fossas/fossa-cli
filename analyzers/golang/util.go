package golang

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/pkg"
)

// Errors that occur while running utilities.
var (
	ErrNoGOPATH = errors.New("no $GOPATH set")
)

// Dir returns the absolute path to a Go package.
func (a *Analyzer) Dir(importpath string) (string, error) {
	pkg, err := a.Go.ListOne(importpath, nil)
	if err != nil {
		return "", err
	}
	return pkg.Dir, nil
}

// ImportPath returns the import path of a package located at the directory.
func ImportPath(dir string) (string, error) {
	if os.Getenv("GOPATH") == "" {
		return "", ErrNoGOPATH
	}
	gopath, err := filepath.Abs(os.Getenv("GOPATH"))
	if err != nil {
		return "", errors.Wrap(err, "could not get absolute $GOPATH")
	}
	importpath, err := filepath.Rel(filepath.Join(gopath, "src"), dir)
	if err != nil {
		return "", errors.Wrap(err, "could not compute import prefix")
	}
	return importpath, nil
}

// Unvendor takes a vendorized import path and strips all vendor folder
// prefixes.
func Unvendor(importpath string) string {
	sections := strings.Split(importpath, "/vendor/")
	return sections[len(sections)-1]
}

// VendorParent returns the directory that contains a vendored directory, or "."
// if none exists.
func VendorParent(dirname string) string {
	separator := filepath.FromSlash("/vendor/")
	if !strings.Contains(dirname, separator) {
		return "."
	}
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
