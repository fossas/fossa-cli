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
func PackageFromManifest(dir string, manifestFileName string) (pkg.Package, error) {
	var manifest manifest

	filePath := filepath.Join(dir, manifestFileName)

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

	transitiveDeps, err := fromSubNodeModules(dir, dir, rootPackage)

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

func convertManifestToPkg(manifest manifest) pkg.Package {
	id := pkg.ID{
		Type:     pkg.NodeJS,
		Name:     manifest.Name,
		Revision: manifest.Version,
	}

	var imports pkg.Imports
	for depName, version := range manifest.Dependencies {
		imports = append(imports, createImport(depName, version))
	}

	return pkg.Package{
		ID:      id,
		Imports: imports,
	}
}

func createImport(packageName string, version string) pkg.Import {
	id := pkg.ID{
		Type:     pkg.NodeJS,
		Name:     packageName,
		Revision: version,
	}

	return pkg.Import{
		Target:   packageName,
		Resolved: id,
	}
}

func fromSubNodeModules(currentDir string, rootNodeModuleDir string, previousPackage pkg.Package) (map[pkg.ID]pkg.Package, error) {
	submoduleProjects := make(map[pkg.ID]pkg.Package)

	for i, imported := range previousPackage.Imports {
		validSubmodulePath, err := findValidSubModulePath(imported.Target, currentDir, rootNodeModuleDir)
		if err != nil {
			return nil, err
		}

		validSubmodulePath = filepath.Join(validSubmodulePath, imported.Target)

		subProject, err := PackageFromManifest(validSubmodulePath, "package.json")
		if err != nil {
			return nil, err
		}

		submoduleProjects[subProject.ID] = subProject

		// update previous project's revision resolved reference to stamp out non-deterministic behavior (e.g. semver defined versions in package.json)
		previousPackage.Imports[i].Resolved.Revision = subProject.ID.Revision

		nextLevelSubModules, err := fromSubNodeModules(validSubmodulePath, rootNodeModuleDir, subProject)
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

func findValidSubModulePath(submodule string, currentDir string, rootNodeModuleDir string) (string, error) {
	// check n + 1 depth
	path, err := submoduleByNodeModules(currentDir, submodule)
	if err != nil {
		return "", err
	}

	if path != "" {
		return path, nil
	}

	path, err = submoduleByNodeModules(rootNodeModuleDir, submodule)
	if err != nil {
		return "", err
	}

	return path, nil
}

func submoduleByNodeModules(currentDir string, submodule string) (string, error) {
	moduleExists, err := files.ExistsFolder(currentDir, "node_modules", submodule)
	if err != nil || !moduleExists {
		return "", err
	}

	return filepath.Join(currentDir, "node_modules"), nil
}
