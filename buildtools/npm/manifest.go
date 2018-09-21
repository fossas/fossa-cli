package npm

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Manifest struct {
	Name         string
	Version      string
	Dependencies map[string]string
}

// FromManifest creates a manifest from the filepath provided
func FromManifest(pathElems ...string) (Manifest, error) {
	var manifest Manifest

	filePath := filepath.Join(pathElems...)

	err := files.ReadJSON(&manifest, filePath)
	if err != nil {
		return Manifest{}, err
	}

	return manifest, nil
}

// PackageFromManifest generates a package definition for the provided manifest in the supplied directory. Performs revision resolution
func PackageFromManifest(pathElems ...string) (pkg.Package, error) {
	filePath := filepath.Join(pathElems...)
	manifest, err := FromManifest(filePath)
	if err != nil {
		return pkg.Package{}, err
	}

	manifestAsPackage := convertManifestToPkg(manifest)

	// attempt to resolve revisions if node_modules folder exists and package any imports
	if len(manifestAsPackage.Imports) == 0 {
		return manifestAsPackage, nil
	}

	nodeModuleDirectory := filepath.Join(filepath.Dir(filePath), "node_modules")
	nodeModulesExists, err := files.ExistsFolder(nodeModuleDirectory)
	if err != nil {
		return pkg.Package{}, err
	}

	if !nodeModulesExists {
		return manifestAsPackage, nil
	}

	err = resolveDirectDependencyVersions(&manifestAsPackage, nodeModuleDirectory)
	if err != nil {
		return pkg.Package{}, err
	}
	if !nodeModulesExists {
		return manifestAsPackage, nil
	}

	return manifestAsPackage, nil
}

// FromNodeModules generates the dep graph based on the manifest provided at the supplied path
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

	transitiveDeps := make(map[pkg.ID]pkg.Package)

	err = fromModulesHelper(manifestPath, transitiveDeps)

	// The root package also get's bundled in, but it is not a dep of itself, so remove it
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

// convertManifestToPkg converts a given manifest to a package. Does not resolve unresolved imports
func convertManifestToPkg(manifest Manifest) pkg.Package {
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

/*
	1. get package at pathToModule
	2. add  currentPackage to the accumulator
	3. for each of the dependencies found in currentPackage
		3a. determine the correct path to that module
		3b. create a package for that dependency and add it to the accumulator
		3c. recurse to 1 using the path to the dependency
*/
func fromModulesHelper(pathToModule string, moduleProjects map[pkg.ID]pkg.Package) error {
	currentDir := filepath.Dir(pathToModule)

	currentModule, err := PackageFromManifest(pathToModule)
	if err != nil {
		return err
	}

	moduleProjects[currentModule.ID] = currentModule

	for _, imported := range currentModule.Imports {
		currentDirWithNodeModules := filepath.Join(currentDir, "node_modules")
		pathToDepModule, err := modulePath(currentDirWithNodeModules, imported.Target)
		if err != nil {
			return err
		}

		modulePackage, err := PackageFromManifest(pathToDepModule) //, "package.json")
		if err != nil {
			return err
		}

		moduleProjects[modulePackage.ID] = modulePackage

		err = fromModulesHelper(pathToDepModule, moduleProjects)
		if err != nil {
			return err
		}
	}

	return nil
}

func resolveDirectDependencyVersions(providedPackge *pkg.Package, nodeModuleDirectory string) error {
	for i, directDep := range providedPackge.Imports {
		directDepPath, err := modulePath(nodeModuleDirectory, directDep.Target)
		if err != nil {
			return err
		}

		directDepManifest, err := FromManifest(directDepPath)
		if err != nil {
			return err
		}

		providedPackge.Imports[i].Resolved.Revision = directDepManifest.Version
	}

	return nil
}
