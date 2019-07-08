package cargo

import (
	"path/filepath"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type manifest struct {
	Workspace         workspace
	Dependencies      map[string]interface{} `toml:"dependencies"`
	BuildDependencies map[string]interface{} `toml:"build-dependencies"`
	Package           Package
}

type workspace struct {
	Members []string
}

type lockfile struct {
	Packages []Package `toml:"package"`
}

// Package is a single imported repository within a dep project.
type Package struct {
	Name         string
	Dependencies []string
	Version      string
	Source       string
}

// LockfileDependencies uses a lockfile and the associated manifest file to create a dependency
// graph. This method approximates the direct dependencies by selecting all dependencies
// in the manifest file as direct dependencies and determines the version from the lockfile.
func LockfileDependencies(lockfilePath string, dir string) (graph.Deps, error) {
	var lock lockfile
	err := files.ReadTOML(&lock, filepath.Join(dir, lockfilePath))
	if err != nil {
		return graph.Deps{}, errors.Wrap(err, "No lockfile Cargo.lock found")
	}

	IDMap := make(map[string]pkg.ID)
	for _, dep := range lock.Packages {
		IDMap[dep.Name] = pkg.ID{
			Type:     pkg.Rust,
			Name:     dep.Name,
			Revision: dep.Version,
			Location: dep.Source,
		}
	}

	transitiveDependenices := make(map[pkg.ID]pkg.Package)
	for _, dep := range lock.Packages {
		imports := []pkg.Import{}
		for _, trans := range dep.Dependencies {
			transDep := strings.Split(trans, " ")
			imports = append(imports, pkg.Import{
				Target:   transDep[0],
				Resolved: IDMap[transDep[0]],
			})
		}

		transitiveDependenices[IDMap[dep.Name]] = pkg.Package{
			ID:      IDMap[dep.Name],
			Imports: imports,
		}
	}

	manifests := []manifest{}
	var man manifest
	err = files.ReadTOML(&man, filepath.Join(dir, "Cargo.toml"))
	if err != nil {
		log.Warnf("manifest file `Cargo.toml` was not found in directory: %s. direct dependencies may be incorrect. error: %+v", dir, err)
	}
	manifests = append(manifests, man)

	for _, member := range man.Workspace.Members {
		memberManifest := manifest{}
		memberFile := filepath.Join(dir, member, "Cargo.toml")
		err = files.ReadTOML(&memberManifest, memberFile)
		if err != nil {
			log.Warnf("member manifest file `%s` was not able to be read. direct dependencies may be incorrect. error: %+v", memberFile, err)
			continue
		}
		manifests = append(manifests, memberManifest)
	}

	imports := []pkg.Import{}
	for _, manifest := range manifests {
		for dep := range manifest.Dependencies {
			imports = append(imports, pkg.Import{
				Target:   dep,
				Resolved: IDMap[dep],
			})
		}
		for dep := range manifest.BuildDependencies {
			imports = append(imports, pkg.Import{
				Target:   dep,
				Resolved: IDMap[dep],
			})
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transitiveDependenices,
	}, nil
}
