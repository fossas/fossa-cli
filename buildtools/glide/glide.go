// Package glide provides functions for working with the glide tool.
package glide

import (
	"errors"
	"path"
	"path/filepath"
	"time"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

// ErrNoLockfile is returned if a glide manifest is found without an accompanying lockfile.
var ErrNoLockfile = errors.New("found glide.yaml but not glide.lock")

// An Import is a single imported repository within a glide project.
type Import struct {
	Name        string
	Version     string
	Subpackages []string
}

// A Lockfile contains the contents of a glide lockfile. Lockfiles are resolvers.
type Lockfile struct {
	Hash        string
	Updated     time.Time
	Imports     []Import
	TestImports []Import

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
		return Lockfile{}, errors.New("directory does not use glide")
	}
	lockfile, err := FromFile(filepath.Join(dirname, "glide.lock"))
	if err != nil {
		return Lockfile{}, err
	}
	normalized := make(map[string]pkg.Import)
	for _, project := range lockfile.Imports {
		for _, subpkg := range project.Subpackages {
			importpath := path.Join(project.Name, subpkg)
			normalized[importpath] = pkg.Import{
				Target: project.Version,
				Resolved: pkg.ID{
					Type:     pkg.Go,
					Name:     importpath,
					Revision: project.Version,
					Location: "",
				},
			}
		}
		// The repository in glide lockfiles might also be a package, but it won't
		// be listed as a _subpackage_.
		normalized[project.Name] = pkg.Import{
			Target: project.Version,
			Resolved: pkg.ID{
				Type:     pkg.Go,
				Name:     project.Name,
				Revision: project.Version,
				Location: "",
			},
		}
	}

	lockfile.normalized = normalized
	return lockfile, nil
}

// UsedIn checks whether glide is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	// Check whether there exists a manifest.
	ok, err := files.Exists(dirname, "glide.yaml")
	if err != nil {
		return false, err
	}

	if ok {
		// Check whether there exists a lockfile.
		ok, err := files.Exists(dirname, "glide.lock")
		if err != nil {
			return false, err
		}
		// If both exist, then glide is being used correctly.
		if ok {
			return true, nil
		}
		// If only a manifest exists, then return ErrNoLockfile.
		return true, ErrNoLockfile
	}
	// If there is no manifest, then glide is not being used.
	return false, nil
}

// FromFile reads a raw Lockfile from a glide.lock file.
func FromFile(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadYAML(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
