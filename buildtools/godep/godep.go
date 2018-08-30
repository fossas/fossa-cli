// Package godep provides functions for working with the godep tool.
package godep

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

// A Package is a single imported package within a godep project.
type Package struct {
	ImportPath string
	Rev        string
	Comment    string
}

// Lockfile contains the contents of a godep lockfile.
type Lockfile struct {
	ImportPath   string
	GoVersion    string
	GodepVersion string
	Deps         []Package

	normalized map[string]pkg.Import
}

// Resolve implements resolver.Resolver for godep.
func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	rev, ok := l.normalized[importpath]
	if !ok {
		return pkg.Import{}, buildtools.ErrNoRevisionForPackage
	}
	return rev, nil
}

// New constructs a godep Lockfile while pre-computing the revision lookup.
func New(dirname string) (Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return Lockfile{}, err
	}
	if !ok {
		return Lockfile{}, errors.New("directory does not use godep")
	}
	lockfile, err := FromFile(filepath.Join(dirname, "Godeps", "Godeps.json"))
	if err != nil {
		return Lockfile{}, err
	}
	normalized := make(map[string]pkg.Import)
	for _, project := range lockfile.Deps {
		normalized[project.ImportPath] = pkg.Import{
			Target: project.Comment,
			Resolved: pkg.ID{
				Type:     pkg.Go,
				Name:     project.ImportPath,
				Revision: project.Rev,
				Location: "",
			},
		}
	}

	lockfile.normalized = normalized
	return lockfile, nil
}

// UsedIn checks whether godep is used correctly within a project folder.
func UsedIn(dirname string) (bool, error) {
	return files.Exists(dirname, "Godeps", "Godeps.json")
}

// FromFile reads a raw Lockfile from a Godeps/Godeps.json file.
func FromFile(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
