package yarn

import (
	"errors"
	"path/filepath"
	"regexp"

	yaml "gopkg.in/yaml.v2"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// FromProject builds a dependency graph based on the provided lockfile and manifest
func FromProject(manifestPath string, lockFilePath string) (graph.Deps, error) {
	rootPackageJsonExists, err := files.Exists(manifestPath)
	if err != nil {
		return graph.Deps{}, err
	}
	if !rootPackageJsonExists {
		return graph.Deps{}, errors.New(manifestPath + " does not exist")
	}

	yarnLockfileExists, err := files.Exists(lockFilePath)
	if err != nil {
		return graph.Deps{}, err
	}
	if !yarnLockfileExists {
		return graph.Deps{}, errors.New(lockFilePath + " does not exist")
	}

	// To know which deps are direct, we need the manifest def
	lockfile, err := readLockfile(lockFilePath)
	if err != nil {
		return graph.Deps{}, err
	}

	manifest, err := npm.FromManifest(manifestPath)
	if err != nil {
		return graph.Deps{}, err
	}

	directDeps := make(pkg.Imports, len(manifest.Dependencies))
	i := 0
	for name, revision := range manifest.Dependencies {
		directDeps[i] = pkg.Import{
			Target: name,
			Resolved: pkg.ID{
				Name:     name,
				Revision: lockfile.resolve(name, revision),
				Type:     pkg.NodeJS,
			},
		}
		i++
	}

	transitiveDepGraph := make(map[pkg.ID]pkg.Package)
	lockfile.resolveDepGraph(manifest.Dependencies, transitiveDepGraph)

	return graph.Deps{
		Direct:     directDeps,
		Transitive: transitiveDepGraph,
	}, nil
}

func readLockfile(pathElems ...string) (yarnLockfile, error) {
	var lockfile yarnLockfile

	filePath := filepath.Join(pathElems...)

	fileContent, err := files.Read(filePath)
	if err != nil {
		return yarnLockfile{}, err
	}

	r, err := regexp.Compile("\\s\"")
	if err != nil {
		return yarnLockfile{}, err
	}
	yamlCompatLockfile := r.ReplaceAll(fileContent, []byte(": \""))

	err = yaml.Unmarshal(yamlCompatLockfile, &lockfile)
	if err != nil {
		return yarnLockfile{}, err
	}

	return lockfile, nil
}

type lockfileEntry struct {
	// resolved version for the particular entry based on the provided semver revision
	Version string
	// the list of unresolved modules and revisions (e.g. type-detect : ^4.0.0)
	Dependencies map[string]string
}

// Keys for each lockfile entry follow the schema <moduleName>@<semverVersion>  (e.g. type-detect@^4.0.0)
type yarnLockfile map[string]lockfileEntry

func (l yarnLockfile) resolve(moduleName string, unresolvedRevision string) string {
	return l[moduleName+"@"+unresolvedRevision].Version
}

func (l yarnLockfile) resolveDepGraph(unresolvedDirectDeps map[string]string, depGraph map[pkg.ID]pkg.Package) {
	for directDepName, unresolvedVersion := range unresolvedDirectDeps {
		lockfileKey := directDepName + "@" + unresolvedVersion

		entry := l[lockfileKey]

		// Create package key for current dep
		pkgID := pkg.ID{
			Name:     directDepName,
			Revision: entry.Version,
			Type:     pkg.NodeJS,
		}

		// Build direct deps for current import
		imports := make(pkg.Imports, len(entry.Dependencies))
		i := 0
		for name, semverRevision := range entry.Dependencies {
			imports[i] = pkg.Import{
				Target: name,
				Resolved: pkg.ID{
					Name:     name,
					Revision: l.resolve(name, semverRevision),
					Type:     pkg.NodeJS,
				},
			}
			i++
		}

		// Add new package to dep graph
		depGraph[pkgID] = pkg.Package{
			ID:       pkgID,
			Imports:  imports,
			Strategy: "yarn-lockfile",
		}

		// recurse for the n-2 direct deps
		l.resolveDepGraph(entry.Dependencies, depGraph)
	}
}
