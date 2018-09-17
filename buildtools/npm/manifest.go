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
func FromNodeModules(dir string, manifestFileName string) (graph.Deps, error) {
	exists, err := files.Exists(dir, manifestFileName)
	if err != nil {
		return graph.Deps{}, err
	} else if !exists {
		return graph.Deps{}, errors.New("no package.json at root of node project")
	}

	rootPackage, err := PackageFromManifest(dir, manifestFileName)
	if err != nil {
		return graph.Deps{}, err
	}

	transitiveDeps, err := fromModulesHelper(dir, rootPackage)

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
func subModulePath(moduleName string, startingDir string) (string, error) {
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

	return filepath.Join(filePath, moduleName), err
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

func fromModulesHelper(currentDir string, previousPackage pkg.Package) (map[pkg.ID]pkg.Package, error) {
	submoduleProjects := make(map[pkg.ID]pkg.Package)

	for i, imported := range previousPackage.Imports {
		pathToSubModule, subProject, err := subModule(imported.Target, filepath.Join(currentDir, "node_modules"))
		if err != nil {
			return nil, err
		}

		submoduleProjects[subProject.ID] = subProject

		// update previous project's revision resolved reference to stamp out non-deterministic behavior (e.g. semver defined versions in package.json)
		previousPackage.Imports[i].Resolved.Revision = subProject.ID.Revision

		nextLevelSubModules, err := fromModulesHelper(pathToSubModule, subProject)
		if err != nil {
			return nil, err
		}

		for _, nextLevelSubModule := range nextLevelSubModules {
			submoduleProjects[nextLevelSubModule.ID] = nextLevelSubModule
		}
	}

	return submoduleProjects, nil
}

func subModule(moduleName string, currentDir string) (pathToSubModule string, modulePackage pkg.Package, err error) {
	pathToSubModule, err = subModulePath(moduleName, currentDir)
	println(pathToSubModule)
	if err != nil {
		return "", pkg.Package{}, err
	}

	modulePackage, err = PackageFromManifest(pathToSubModule, "package.json")
	if err != nil {
		return "", pkg.Package{}, err
	}

	return
}
