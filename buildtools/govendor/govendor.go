// Package govendor provides tools for working with govendor.
package govendor

import (
	"errors"
	"path"
	"path/filepath"
	"time"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
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
			rev.Resolved.Name = importpath
			return rev, nil
		}
	}
	return pkg.Import{}, buildtools.ErrNoRevisionForPackage
}

func New(dirname string) (Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return Lockfile{}, err
	}
	if !ok {
		return Lockfile{}, errors.New("directory does not use govendor")
	}
	lockfile, err := FromFile(filepath.Join(dirname, "vendor", "vendor.json"))
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

// FromFile reads a raw Lockfile from a vendor/vendor.json file.
func FromFile(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
