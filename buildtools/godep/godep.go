// Package godep provides functions for working with the godep tool.
package godep

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/errutil"
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

func (l Lockfile) Resolve(importpath string) (pkg.Import, error) {
	rev, ok := l.normalized[importpath]
	if !ok {
		return pkg.Import{}, errutil.ErrNoRevisionForPackage
	}
	return rev, nil
}

func New(dirname string) (Lockfile, error) {
	ok, err := UsedIn(dirname)
	if err != nil {
		return Lockfile{}, err
	}
	if !ok {
		return Lockfile{}, errors.New("directory does not use godep")
	}
	lockfile, err := ReadRaw(filepath.Join(dirname, "Godeps", "Godeps.json"))
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

// Read returns the contents of a godep project.
func Read(dirname string) ([]Package, error) {
	return ReadFile(filepath.Join(dirname, "Godeps", "Godeps.json"))
}

// ReadFile returns the contents of a godep lockfile.
func ReadFile(filename string) ([]Package, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return nil, err
	}
	return lockfile.Deps, nil
}

// ReadRaw reads a raw Lockfile from a Godeps/Godeps.json file.
func ReadRaw(filename string) (Lockfile, error) {
	var lockfile Lockfile
	err := files.ReadJSON(&lockfile, filename)
	if err != nil {
		return Lockfile{}, err
	}
	return lockfile, nil
}
