// Package dep provides functions for working with the dep tool.
package dep

import (
	"path"
	"strings"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Resolver contains both the lockfile and manifest information. Resolver implements golang.Resolver.
type Resolver struct {
	manifest manifest
	lockfile lockfile
}

// manifest contains the ignored packages in a dep toml file. manifest implements golang.Resolver.
type manifest struct {
	Ignored    []string
	Constraint []Project
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

// New constructs a golang.Resolver given the path to the manifest and lockfile.
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

// LockfileGraph reads a dep lockfile and creates a dependency graph.
func LockfileGraph(filepath string) (graph.Deps, error) {
	file := filepath
	if file == "" {
		file = "Gopkg.lock"
	}

	lock, err := readLockfile(file)
	if err != nil {
		return graph.Deps{}, err
	}

	depGraph := graph.Deps{Transitive: make(map[pkg.ID]pkg.Package)}
	for _, project := range lock.Projects {
		version := project.Version
		if version == "" {
			version = project.Revision
		}

		ID := pkg.ID{
			Type:     pkg.Go,
			Name:     project.Name,
			Revision: version,
		}
		depGraph.Direct = append(depGraph.Direct, pkg.Import{
			Target:   ID.Name,
			Resolved: ID,
		})
		depGraph.Transitive[ID] = pkg.Package{ID: ID}
	}

	return depGraph, nil
}

// ManifestGraph reads a dep manifest and creates a dependency graph.
func ManifestGraph(filepath string) (graph.Deps, error) {
	file := filepath
	if file == "" {
		file = "Gopkg.toml"
	}

	man, err := readManifest(file)
	if err != nil {
		return graph.Deps{}, err
	}

	depGraph := graph.Deps{Transitive: make(map[pkg.ID]pkg.Package)}
	for _, project := range man.Constraint {
		version := project.Version
		if version == "" {
			version = project.Revision
		}

		ID := pkg.ID{
			Type:     pkg.Go,
			Name:     project.Name,
			Revision: version,
		}
		depGraph.Direct = append(depGraph.Direct, pkg.Import{
			Target:   ID.Name,
			Resolved: ID,
		})
		depGraph.Transitive[ID] = pkg.Package{ID: ID}
	}

	return depGraph, nil
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
			revision := project.Version
			if revision == "" {
				revision = project.Revision
			}
			importpath := path.Join(project.Name, pk)
			normalized[importpath] = pkg.Import{
				Target: project.Version,
				Resolved: pkg.ID{
					Type:     pkg.Go,
					Name:     importpath,
					Revision: revision,
					Location: "",
				},
			}
		}
	}

	lock.normalized = normalized
	return lock, nil
}

// readManifest returns a manifest using the provided filepath.
func readManifest(filepath string) (manifest, error) {
	var man manifest
	err := files.ReadTOML(&man, filepath)
	if err != nil {
		return manifest{}, errors.Wrap(err, "No manifest Gopkg.toml found")
	}

	return man, nil
}
