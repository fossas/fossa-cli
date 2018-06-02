// Package dep provides functions for working with the dep tool.
package dep

import (
	"errors"
	"path/filepath"

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
