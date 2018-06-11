// Package dep provides functions for working with the dep tool.
package dep

import (
	"errors"
	"path"
	"path/filepath"

	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/files"
)

// ErrNoLockfile is returned if a dep manifest is found without an accompanying lockfile.
var ErrNoLockfile = errors.New("found Gopkg.toml but not Gopkg.lock")

// A Project is a single imported repository within a dep project.
type Project struct {
	Name     string
	Packages []string
	Revision string
	Version  string
}

// A Lockfile contains the contents of a dep lockfile.
type Lockfile struct {
	Projects []Project

	normalized map[string]string // A normalized map of package import paths to revisions.
}

// New constructs a golang.Resolver
func New(dirname string) (Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return Lockfile{}, err
	}
	if !ok {
		return Lockfile{}, errors.New("directory does not use dep")
	}
	lockfile, err := ReadRaw(filepath.Join(dirname, "Gopkg.lock"))
	if err != nil {
		return Lockfile{}, err
	}
	normalized := make(map[string]string)
	for _, project := range lockfile.Projects {
		for _, pkg := range project.Packages {
			normalized[path.Join(project.Name, pkg)] = project.Revision
		}
	}
	return Lockfile{
		Projects:   lockfile.Projects,
		normalized: normalized,
	}, nil
}

// Resolve returns the revision of an imported Go package contained within the
// lockfile. If the package is not found, errutil.ErrNoRevisionForPackage is
// returned.
func (l Lockfile) Resolve(importpath string) (string, error) {
	rev, ok := l.normalized[importpath]
	if !ok {
		return "", errutil.ErrNoRevisionForPackage
	}
	return rev, nil
}

// UsedIn checks whether dep is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	// Check whether there exists a manifest.
	ok, err := files.Exists(dirname, "Gopkg.toml")
	if err != nil {
		return false, err
	}

	if ok {
		// Check whether there exists a lockfile.
		ok, err := files.Exists(dirname, "Gopkg.lock")
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

// Read returns the contents of a dep project.
func Read(dirname string) ([]Project, error) {
	return ReadFile(filepath.Join(dirname, "Gopkg.lock"))
}

// ReadFile returns the contents of a dep lockfile.
func ReadFile(filename string) ([]Project, error) {
	var lockfile Lockfile
	err := files.ReadTOML(&lockfile, filename)
	if err != nil {
		return nil, err
	}
	return lockfile.Projects, nil
}

// ReadRaw reads a raw Lockfile from a Gopkg.lock file.
func ReadRaw(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadTOML(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
