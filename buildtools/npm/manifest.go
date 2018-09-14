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

// generates a package definition for the provided manifest in the supplied directory
func PackageFromManifest(pathElems ...string) (pkg.Package, error) {
	var manifest manifest

	filePath := filepath.Join(pathElems...)

	err := files.ReadJSON(&manifest, filePath)
	if err != nil {
		return pkg.Package{}, err
	}

	return convertManifestToPkg(manifest), nil
}

// Generates the dep graph based on the manifest provided in the supplied directory
func FromNodeModules(dir string, manifestFileName string) (graph.Deps, error) {
	exists, err := files.Exists(dir, manifestFileName)
	if err != nil {
		return graph.Deps{}, err
	} else if !exists {
		return graph.Deps{}, errors.New("No package.json at root of node project")
	}

	rootPackage, err := PackageFromManifest(dir, manifestFileName)
	if err != nil {
		return graph.Deps{}, err
	}

	transitiveDeps, err := fromSubNodeModules(dir, rootPackage)

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

func subModulePath(moduleName string, currentDir string) (string, error) {
	var modulePath string
	parentPath := filepath.Dir(currentDir)
	if filepath.Base(currentDir) == "node_modules" {
		modulePath = filepath.Join(currentDir, moduleName)

		// remove extra node_modules
		parentPath = filepath.Dir(parentPath)
	} else {
		modulePath = filepath.Join(currentDir, "node_modules", moduleName)
	}

	moduleFolderExists, err := files.ExistsFolder(modulePath)
	if err != nil {
		return "", err
	}

	if moduleFolderExists {
		return modulePath, nil
	}

	if parentPath == currentDir {
		return "", errors.New("cannot find source of module " + moduleName)
	}

	return subModulePath(moduleName, parentPath)
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

func fromSubNodeModules(currentDir string, previousPackage pkg.Package) (map[pkg.ID]pkg.Package, error) {
	submoduleProjects := make(map[pkg.ID]pkg.Package)

	for i, imported := range previousPackage.Imports {
		pathToSubModule, subProject, err := subModule(imported.Target, currentDir)

		submoduleProjects[subProject.ID] = subProject

		// update previous project's revision resolved reference to stamp out non-deterministic behavior (e.g. semver defined versions in package.json)
		previousPackage.Imports[i].Resolved.Revision = subProject.ID.Revision

		nextLevelSubModules, err := fromSubNodeModules(pathToSubModule, subProject)
		if err != nil {
			return nil, err
		}

		for pkgID, nextLevelSubModule := range nextLevelSubModules {
			submoduleProjects[nextLevelSubModule.ID] = nextLevelSubModule
			submoduleProjects[pkgID] = nextLevelSubModule
		}
	}

	return submoduleProjects, nil
}

func subModule(moduleName string, currentDir string) (pathToSubModule string, modulePackage pkg.Package, err error) {
	pathToSubModule, err = subModulePath(moduleName, currentDir)
	if err != nil {
		return "", pkg.Package{}, err
	}

	modulePackage, err = PackageFromManifest(pathToSubModule, "package.json")
	if err != nil {
		return "", pkg.Package{}, err
	}

	return
}
