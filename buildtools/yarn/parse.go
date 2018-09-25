package yarn

import (
	"errors"
	"path/filepath"
	"regexp"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
	"gopkg.in/yaml.v2"
)

type lockfileEntry struct {
	Version      string
	Dependencies map[string]string
}

type YarnLockfile map[string]lockfileEntry

// FromLockfile builds a dependency graph based on the provided path to yarn.lock
func FromLockfile(pathElems ...string) (graph.Deps, error) {
	filePath := filepath.Join(pathElems...)
	yarnLockfileExists, err := files.Exists(filePath)
	if err != nil {
		return graph.Deps{}, err
	}
	if !yarnLockfileExists {
		return graph.Deps{}, errors.New(filePath + " does not exist")
	}

	// To know which deps are direct, we need the manifest def
	manifestPath := filepath.Join(filepath.Dir(filePath), "package.json")
	// manifest, err := npm.FromManifest(manifestPath)
	// if err != nil {
	// 	return graph.Deps{}, nil
	// }
	// println(manifest.Dependencies)

	lockfile, err := readLockfile(filePath)
	if err != nil {
		return graph.Deps{}, nil
	}

	for name, entry := range lockfile {
		println(name + "->" + entry.Version)
		for depName, depVersion := range entry.Dependencies {
			println("\t" + depName + "@" + depVersion)
		}
	}

	directDeps, err := directDepsFromManifest(manifestPath)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     directDeps,
		Transitive: make(map[pkg.ID]pkg.Package),
	}, nil

	// return depGraph(rootPkg.Imports, lockfile)
}

func directDepsFromManifest(manifestPath string) (pkg.Imports, error) {
	manifest, err := npm.FromManifest(manifestPath)
	if err != nil {
		return pkg.Imports{}, nil
	}

	directDeps := make(pkg.Imports, len(manifest.Dependencies))

	i := 0
	for name, revision := range manifest.Dependencies {
		directDeps[i] = pkg.Import{
			Target: name,
			Resolved: pkg.ID{
				Name:     name,
				Revision: revision,
				Type:     pkg.NodeJS,
			},
		}
		i++
	}

	return directDeps, nil
}

// func depGraph(directDeps pkg.Imports, lockfile YarnLockfile) (graph.Deps, error) {
// }

func readLockfile(pathElems ...string) (YarnLockfile, error) {
	var lockfile YarnLockfile

	filePath := filepath.Join(pathElems...)

	fileContent, err := files.Read(filePath)
	if err != nil {
		return YarnLockfile{}, err
	}

	r, err := regexp.Compile("\\s\"")
	yamlCompatLockfile := r.ReplaceAll(fileContent, []byte(": \""))
	println(string(fileContent))

	err = yaml.Unmarshal(yamlCompatLockfile, &lockfile)
	if err != nil {
		return YarnLockfile{}, err
	}

	return lockfile, nil
}
