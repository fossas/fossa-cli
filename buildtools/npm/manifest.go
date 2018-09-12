package npm

import (
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
)

type Manifest struct {
	Name         string
	Version      string
	Dependencies map[string]string
}

func FromManifest(filename string) (Manifest, error) {
	var manifest Manifest
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return Manifest{}, err
	}
	return manifest, nil
}

func FromNodeModules(dir string) ([]Manifest, error) {
	if !strings.HasSuffix(dir, "node_modules") {
		dir = filepath.Join(dir, "node_modules")
	}

	manifests := make([]Manifest, 0)
	dirNames, err := files.DirectoryNames(dir)

	if err != nil {
		return manifests, err
	}

	for _, dirName := range dirNames {
		manifestFile := filepath.Join(dir, dirName, "package.json")
		manifestExists, err := files.Exists(manifestFile)

		if manifestExists && err == nil {
			manifest, err := FromManifest(manifestFile)

			if err == nil {
				manifests = append(manifests, manifest)
			}
		}
	}

	return manifests, nil
}

// func FromNodeModules(dir string) ([]Manifest, error) {
// 	manifests := make([]Manifest, 0)

// 	nodeModulesFolderExists, err := dirHasNodeModulesFolder(dir)
// 	if err != nil {
// 		return manifests, err
// 	}

// 	if !nodeModulesFolderExists {
// 		return
// 	}

// 	return manifests, nil
// }

type Lockfile struct {
	Dependencies map[string]struct {
		Version  string
		Requires map[string]string
	}
}

func FromLockfile(filename string) (Lockfile, error) {
	return Lockfile{}, errors.ErrNotImplemented
}

func dirHasPackageJson(dir string) (bool, error) {
	return files.Exists(dir, "package.json")
}

func dirHasNodeModulesFolder(dir string) (bool, error) {
	return files.ExistsFolder(dir, "node_modules")
}
