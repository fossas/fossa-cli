// Package godep provides functions for working with the godep tool.
package godep

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/files"
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
