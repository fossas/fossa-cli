// Package dep provides functions for working with the dep tool.
package dep

import (
	"path"
	"strings"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/pkg/errors"
)

// Resolver contains both the lockfile and Manifest information. Resolver implements golang.Resolver.
type Resolver struct {
	Manifest Manifest
	lockfile lockfile
}

// Manifest contains the ignored packages in a dep toml file. Manifest implements golang.Resolver.
type Manifest struct {
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
	if r.Manifest.isIgnored(importpath) {
		return pkg.Import{}, buildtools.ErrPackageIsIgnored
	}

	revision, ok := r.lockfile.normalized[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}

	return revision, nil
}

// isIgnored checks if a Go package can be ignored according to a dep manifest.
func (m Manifest) isIgnored(importpath string) bool {
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

// New constructs a golang.Resolver given the path to the manifest and lockfile.
func New(lockfilePath string, manifestPath string) (Resolver, error) {
	var err error

	resolver := Resolver{}
	resolver.lockfile, err = readLockfile(lockfilePath)
	if err != nil {
		return Resolver{}, err
	}

	resolver.Manifest, err = readManifest(manifestPath)
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

	lock.normalized = normalized
	return lock, nil
}

// readManifest returns a Manifest using the provided filepath.
func readManifest(filepath string) (Manifest, error) {
	var manifest Manifest
	err := files.ReadTOML(&manifest, filepath)
	if err != nil {
		return Manifest{}, errors.Wrap(err, "No manifest Gopkg.toml found")
	}

	return manifest, nil
}
