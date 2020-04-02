package npm

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

var PossibleLockfileFilenames = []string{"npm-shrinkwrap.json", "package-lock.json"}

type Lockfile struct {
	Name         string
	Version      string
	Dependencies Dependencies
}

type DependencyLockEntry struct {
	Version      string
	Requires     map[string]string
	Dependencies Dependencies
}

type Dependencies map[string]*DependencyLockEntry
type DependencyMap map[pkg.ID][]pkg.ID
type DeepDepIdsSet map[pkg.ID]bool

// FindAndReadLockfile checks the root of the node project at the given path for lockfiles, then returns the parsed
// contents if one is found.
func FindAndReadLockfile(path string) (Lockfile, error) {
	var lockfile Lockfile

	for _, filename := range PossibleLockfileFilenames {
		filePath := filepath.Join(path, filename)
		exists, err := files.Exists(filePath)

		if err == nil && exists {
			err := files.ReadJSON(&lockfile, filePath)
			return lockfile, err
		}
	}

	return lockfile, errors.Errorf("none of %v found at root of node project", PossibleLockfileFilenames)
}

// retrieveTransitiveInformation recursively scans a lockfile to construct a map of package IDs to their requirements
// as well as a set of the package IDs that have been required by other dependencies somewhere else in the file.
func retrieveTransitiveInformation(deps Dependencies) (DependencyMap, DeepDepIdsSet) {
	depMap := DependencyMap{}
	deepDepIds := DeepDepIdsSet{}

	for name, info := range deps {
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     name,
			Revision: info.Version,
		}

		// save IDs of dependencies that are required by this dependency
		depMap[id] = []pkg.ID{}
		for depName, version := range info.Requires {
			depId := pkg.ID{
				Type:     pkg.NodeJS,
				Name:     depName,
				Revision: version,
			}
			depMap[id] = append(depMap[id], depId)
			deepDepIds[depId] = true
		}

		// recurse if this dependency has its own "dependencies" map
		innerDepMap, moreDeepDepIds := retrieveTransitiveInformation(info.Dependencies)
		for k, v := range innerDepMap {
			depMap[k] = v
		}
		for k := range moreDeepDepIds {
			deepDepIds[k] = true
		}
	}

	return depMap, deepDepIds
}

// FromLockfile generates the dep graph based on the lockfile provided at the supplied path.
func FromLockfile(path string, devDeps bool) (graph.Deps, error) {
	lockfile, err := FindAndReadLockfile(path)
	if err != nil {
		return graph.Deps{}, err
	}

	depMap, deepDepIds := retrieveTransitiveInformation(lockfile.Dependencies)

	directDeps := pkg.Imports{}
	transitiveDeps := map[pkg.ID]pkg.Package{}

	for id, depIds := range depMap {
		// this ID is a direct dependency if it's not a deep dependency
		// NOTE: this implies that direct deps that are also deep deps aren't considered direct
		if _, ok := deepDepIds[id]; !ok {
			directDeps = append(directDeps, pkg.Import{
				Target:   id.Name,
				Resolved: id,
			})
		}

		// gather transitive dependencies for ID
		var imports pkg.Imports
		for _, depId := range depIds {
			imports = append(imports, pkg.Import{
				Target:   depId.Name,
				Resolved: depId,
			})
		}
		transitiveDeps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	return graph.Deps{
		Direct:     directDeps,
		Transitive: transitiveDeps,
	}, nil
}
