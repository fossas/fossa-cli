// Package dep provides functions for working with the dep tool.
package dep

import (
	"fmt"
	"path"
	"strings"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/pkg/errors"
)

// Resolver contains both the lockfile and manifest information. Resolver implements golang.Resolver.
type Resolver struct {
	manifest manifest
	lockfile lockfile
}

// manifest contains the ignored packages in a dep toml file. manifest implements golang.Resolver.
type manifest struct {
	Ignored []string
}

// lockfile contains the Projects in a dep lockfile and a corresponding map for retrieving project data.
type lockfile struct {
	Projects   []Project
	normalized map[string]pkg.Import
}

// Project is a single imported repository within a dep project.
type Project struct {
	Name     string
	Packages []string
	Revision string
	Version  string
}

// Resolve returns the revision of an imported Go package contained within the
// lockfile and checks to see if it should be ignored. If the package cannot be
// ignored and is not found, buildtools.ErrNoRevisionForPackage is returned.
func (r Resolver) Resolve(importpath string) (pkg.Import, error) {
	if r.manifest.isIgnored(importpath) {
		return pkg.Import{}, buildtools.ErrPackageIsIgnored
	}

	revision, ok := r.lockfile.normalized[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}

	return revision, nil
}

// isIgnored checks if a Go package can be ignored according to a dep manifest.
func (m manifest) isIgnored(importpath string) bool {
	for _, ignoredPackage := range m.Ignored {
		if strings.HasSuffix(ignoredPackage, "*") {
			ignoredPrefix := ignoredPackage[:len(ignoredPackage)-1]
			if strings.HasPrefix(importpath, ignoredPrefix) {
				return true
			}
		}

		if ignoredPackage == importpath {
			return true
		}
	}

	return false
}

<<<<<<< HEAD
// New constructs a golang.Resolver given the path to the manifest and lockfile.
=======
// Resolve returns the revision of an imported Go package contained within the
// lockfile. If the package is not found, buildtools.ErrNoRevisionForPackage is
// returned.
func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	rev, ok := l.Normalized[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}
	return rev, nil
}

// New constructs a golang.Resolver
>>>>>>> unit testing added for dep package
func New(lockfilePath string, manifestPath string) (Resolver, error) {
	var err error

	resolver := Resolver{}
	resolver.lockfile, err = readLockfile(lockfilePath)
	if err != nil {
		return Resolver{}, err
	}

	resolver.manifest, err = readManifest(manifestPath)
	if err != nil {
		return Resolver{}, err
	}

	return resolver, nil
}

// readLockfile returns a lockfile object using the provided filepath.
func readLockfile(filepath string) (lockfile, error) {
	var lock lockfile

	err := files.ReadTOML(&lock, filepath)
	if err != nil {
		return lockfile{}, errors.Wrap(err, "No lockfile Gopkg.lock found")
	}

	normalized := make(map[string]pkg.Import)
	for _, project := range lock.Projects {
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

	resolver.Lockfile.Normalized = normalized
	return resolver, nil
}

// ReadLockFile accepts the filepath of a lockfile which it parses into a lockfile object
func ReadLockfile(filepath string) (Lockfile, error) {
	var lockfile Lockfile

	err := files.ReadTOML(&lockfile, filepath)
	if err != nil {
		return Lockfile{}, fmt.Errorf("No lockfile Gopkg.lock found: %+v", err)
	}
	return lockfile, nil
}

// ReadManifest accepts the filepath of a manifest which it parses into a manifest object
func ReadManifest(filepath string) (Manifest, error) {
	var manifest Manifest
	err := files.ReadTOML(&manifest, filepath)
	if err != nil {
		return Manifest{}, fmt.Errorf("No manifest Gopkg.toml found: %+v", err)
	}

	return manifest, nil
}
