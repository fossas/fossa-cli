package npm

import (
	"path/filepath"
	"strings"

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
	var directDepsIds []pkg.ID
	transitiveDeps := make(map[pkg.ID]pkg.Package)

	exists, err := files.Exists(dir, "package.json")
	if err != nil {
		return graph.Deps{}, err
	} else if !exists {
		return graph.Deps{}, errors.New("No package.json at root of node project")
	}

	pkg, err := PackageFromManifest(filepath.Join(dir, "package.json"))
	if err != nil {
		return graph.Deps{}, err
	}

	directDeps := pkg.Imports
	for _, dirDep := range directDeps {
		directDepsIds = append(directDepsIds, dirDep.Resolved)
	}

	transitiveDeps, err = fromSubNodeModules(dir, directDepsIds)

	if err != nil {
		return graph.Deps{}, err
	}

	return graph.Deps{
		Direct:     directDeps,
		Transitive: transitiveDeps,
	}, nil
}

func fromSubNodeModules(dir string, submodulesToExclude []pkg.ID) (map[pkg.ID]pkg.Package, error) {
	if !strings.HasSuffix(dir, "/node_modules") {
		dir = filepath.Join(dir, "node_modules")
	}

	return nil, nil
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
