package npm

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type manifest struct {
	Name         string
	Version      string
	Dependencies map[string]string
}

// PackageFromManifest generates a package definition for the provided manifest in the supplied directory
func PackageFromManifest(pathElems ...string) (pkg.Package, error) {
	var manifest manifest

	filePath := filepath.Join(pathElems...)

	err := files.ReadJSON(&manifest, filePath)
	if err != nil {
		return pkg.Package{}, err
	}

	return convertManifestToPkg(manifest), nil
}

// FromNodeModules generates the dep graph based on the manifest provided in the supplied directory
func FromNodeModules(pathElems ...string) (graph.Deps, error) {
	manifestPath := filepath.Join(pathElems...)
	exists, err := files.Exists(manifestPath)
	if err != nil {
		return graph.Deps{}, err
	} else if !exists {
		return graph.Deps{}, errors.New("no package.json at root of node project")
	}

	rootPackage, err := PackageFromManifest(manifestPath)
	if err != nil {
		return graph.Deps{}, err
	}

	// The root package also get's bundled in, but it is not a dep of itself, so remove it
	transitiveDeps, err := fromModulesHelper(manifestPath)

	delete(transitiveDeps, rootPackage.ID)

	if err != nil {
		return graph.Deps{}, err
	}

	return graph.Deps{
		Direct:     rootPackage.Imports,
		Transitive: transitiveDeps,
	}, nil
}

type Lockfile struct {
	Dependencies map[string]struct {
		Version  string
		Requires map[string]string
	}
}

func FromLockfile(filename string) (Lockfile, error) {
	return Lockfile{}, errors.ErrNotImplemented
}

// TODO: add support for NODE_PATH and GLOBAL_FOLDERS.
func modulePath(startingDir string, moduleName string) (string, error) {
	filePath, err := files.WalkUp(startingDir, func(currentDir string) (err error) {
		if filepath.Base(currentDir) == moduleName {
			return files.ErrStopWalk
		}

		exists, err := files.ExistsFolder(filepath.Join(currentDir, moduleName))
		if err != nil {
			return err
		}

		if exists {
			return files.ErrStopWalk
		}

		return nil
	})

	return filepath.Join(filePath, moduleName, "package.json"), err
}

func convertManifestToPkg(manifest manifest) pkg.Package {
	id := pkg.ID{
		Type:     pkg.NodeJS,
		Name:     manifest.Name,
		Revision: manifest.Version,
	}

	var imports pkg.Imports
	for depName, version := range manifest.Dependencies {
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     depName,
			Revision: version,
		}

		depImport := pkg.Import{
			Target:   depName,
			Resolved: id,
		}
		imports = append(imports, depImport)
	}

	return pkg.Package{
		ID:      id,
		Imports: imports,
	}
}

func fromModulesHelper(pathToModule string) (map[pkg.ID]pkg.Package, error) {
	moduleProjects := make(map[pkg.ID]pkg.Package)
	currentDir := filepath.Dir(pathToModule)

	currentModule, err := PackageFromManifest(pathToModule)
	if err != nil {
		println(pathToModule)
		return nil, err
	}

	for i, imported := range currentModule.Imports {
		currentDirWithNodeModules := filepath.Join(currentDir, "node_modules")
		pathToDepModule, err := modulePath(currentDirWithNodeModules, imported.Target)
		if err != nil {
			return nil, err
		}

		modulePackage, err := PackageFromManifest(pathToDepModule) //, "package.json")
		if err != nil {
			return nil, err
		}

		moduleProjects[modulePackage.ID] = modulePackage

		// update previous project's revision resolved reference to stamp out non-deterministic behavior (e.g. semver defined versions in package.json)
		currentModule.Imports[i].Resolved = modulePackage.ID
		println("UPDATING " + currentModule.Imports[i].Target + "@" + currentModule.Imports[i].Resolved.Revision + " to " + modulePackage.ID.Revision)

		moduleProjects[currentModule.ID] = currentModule

		nextLevelSubModules, err := fromModulesHelper(pathToDepModule)
		if err != nil {
			return nil, err
		}

		for _, nextLevelSubModule := range nextLevelSubModules {
			moduleProjects[nextLevelSubModule.ID] = nextLevelSubModule
		}
	}

	return moduleProjects, nil
}
