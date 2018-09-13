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

func PackageFromManifest(filename string) (pkg.Package, error) {
	var manifest manifest
	err := files.ReadJSON(&manifest, filename)
	if err != nil {
		return pkg.Package{}, err
	}

	return convertManifestToPkg(manifest), nil
}

func FromNodeModules(dir string) (graph.Deps, error) {
	transitiveDeps := make(map[pkg.ID]pkg.Package)

	exists, err := files.Exists(dir, "package.json")
	if err != nil {
		return graph.Deps{}, err
	} else if !exists {
		return graph.Deps{}, errors.New("No package.json at root of node project")
	}

	rootPackage, err := PackageFromManifest(filepath.Join(dir, "package.json"))
	if err != nil {
		return graph.Deps{}, err
	}

	directDeps := rootPackage.Imports

	transitiveDeps, err = fromSubNodeModules(dir, rootPackage.ID.Name)
	if err != nil {
		return graph.Deps{}, err
	}

	return graph.Deps{
		Direct:     directDeps,
		Transitive: transitiveDeps,
	}, nil
}

func fromSubNodeModules(dir string, rootProjectName string) (map[pkg.ID]pkg.Package, error) {
	pkgs := make(map[pkg.ID]pkg.Package)
	currentPackage, err := PackageFromManifest(filepath.Join(dir, "package.json"))
	if err != nil {
		return nil, err
	}

	// root package's imports are the graph's direct deps. This function will include all package.jsons except the root project (with this line)
	if rootProjectName != currentPackage.ID.Name {
		pkgs[currentPackage.ID] = currentPackage
	}

	dir = filepath.Join(dir, "node_modules")

	nodeModulesExist, err := files.ExistsFolder(dir)
	if err != nil {
		return nil, nil
	}
	if !nodeModulesExist {
		return pkgs, nil
	}

	subDirNames, err := files.DirectoryNames(dir)
	if err != nil {
		return nil, err
	}

	// base case is no additional layers of node modules, in which case this loops never runs
	for _, subDir := range subDirNames {
		subDirPackages, err := fromSubNodeModules(filepath.Join(dir, subDir), rootProjectName)
		if err != nil {
			return nil, err
		}

		for pid, p := range subDirPackages {
			pkgs[pid] = p
		}
	}

	// this assignment must come after the recursive call

	resolveInstalledVersion(currentPackage, pkgs)

	return pkgs, nil
}

// replaces the potentially semver versions with the explicit version found 1 depth away from the root packages
func resolveInstalledVersion(rootPackage pkg.Package, modules map[pkg.ID]pkg.Package) {
	for i, imported := range rootPackage.Imports {
		for moduleKey := range modules {
			if moduleKey.Name == imported.Target {
				rootPackage.Imports[i].Resolved = moduleKey
			}
		}
	}
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
