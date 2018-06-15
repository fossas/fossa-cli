// Package govendor provides tools for working with govendor.
//
// Govendor is unique among the Go dependency management tools in that it is the
// only tool which does not flatten `vendor/` folders.
package govendor

import (
	"errors"
	"path"
	"path/filepath"
	"time"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
)

// A Package is a single imported package within a govendor project.
type Package struct {
	Path         string
	ChecksumSHA1 string
	Revision     string
	RevisionTime time.Time
	Version      string
	VersionExact string
}

// Lockfile contains the contents of a govendor lockfile.
type Lockfile struct {
	Comment string
	Ignore  string
	Package []Package

	normalized map[string]pkg.Import
}

func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	log.Logger.Debugf("%#v", importpath)
	for p := importpath; p != "." && p != "/"; p = path.Dir(p) {
		log.Logger.Debugf("Trying: %#v", p)
		rev, ok := l.normalized[p]
		if ok {
			return rev, nil
		}
	}
	return pkg.Import{}, errutil.ErrNoRevisionForPackage
}

func New(dirname string) (Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return Lockfile{}, err
	}
	if !ok {
		return Lockfile{}, errors.New("directory does not use govendor")
	}
	lockfile, err := ReadRaw(filepath.Join(dirname, "vendor", "vendor.json"))
	if err != nil {
		return Lockfile{}, err
	}
	normalized := make(map[string]pkg.Import)
	for _, project := range lockfile.Package {
		normalized[project.Path] = pkg.Import{
			Target: project.Version,
			Resolved: pkg.ID{
				Type:     pkg.Go,
				Name:     project.Path,
				Revision: project.Revision,
				Location: "",
			},
		}
	}

	lockfile.normalized = normalized
	return lockfile, nil
}

// UsedIn checks whether govendor is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "vendor", "vendor.json")
}

// Read returns the contents of a govendor project.
func Read(dirname string) ([]Package, error) {
	return ReadFile(filepath.Join(dirname, "vendor", "vendor.json"))
}

// ReadFile returns the contents of a govendor lockfile.
func ReadFile(filename string) ([]Package, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return nil, err
	}
	return lockfile.Package, nil
}

// ReadRaw reads a raw Lockfile from a vendor/vendor.json file.
func ReadRaw(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
