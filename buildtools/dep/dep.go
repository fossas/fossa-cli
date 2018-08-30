// Package dep provides functions for working with the dep tool.
package dep

import (
	"errors"
	"path"
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
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

// A Lockfile contains the contents of a dep lockfile. Lockfiles are resolvers.
type Lockfile struct {
	Projects []Project

	normalized map[string]pkg.Import // A normalized map of package import paths to revisions.
}

// Resolve returns the revision of an imported Go package contained within the
// lockfile. If the package is not found, buildtools.ErrNoRevisionForPackage is
// returned.
func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	rev, ok := l.normalized[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}
	return rev, nil
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
	lockfile, err := FromFile(filepath.Join(dirname, "Gopkg.lock"))
	if err != nil {
		return Lockfile{}, err
	}
	normalized := make(map[string]pkg.Import)
	for _, project := range lockfile.Projects {
		for _, pk := range project.Packages {
			importpath := path.Join(project.Name, pk)
			normalized[importpath] = pkg.Import{
				Target: project.Version,
				Resolved: pkg.ID{
					Type:     pkg.Go,
					Name:     importpath,
					Revision: project.Revision,
					Location: "",
				},
			}
		}
	}

	lockfile.normalized = normalized
	return lockfile, nil
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

// FromFile reads a raw Lockfile from a Gopkg.lock file.
func FromFile(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadTOML(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
