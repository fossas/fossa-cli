package cargo

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type manifest struct {
	Workspace         Workspace
	Dependencies      map[string]interface{} `toml:"dependencies"`
	BuildDependencies map[string]interface{} `toml:"build-dependencies"`
	Package           Package
}

type Workspace struct {
	Members []string
}

// lockfile contains the Projects in a dep lockfile and a corresponding map for retrieving project data.
type lockfile struct {
	Packages []Package `toml:"package"`
}

// Project is a single imported repository within a dep project.
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
	err := files.ReadTOML(&lock, lockfilePath)
	if err != nil {
		return graph.Deps{}, errors.Wrap(err, "No lockfile Gopkg.lock found")
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
		return graph.Deps{}, errors.Wrap(err, "No lockfile Gopkg.lock found")
	}
	manifests = append(manifests, man)
	for _, list := range man.Workspace.Members {
		subMan := manifest{}
		subFile := filepath.Join(dir, list, "Cargo.toml")
		err = files.ReadTOML(&subMan, subFile)
		manifests = append(manifests, subMan)
	}

	imports := []pkg.Import{}
	for _, mani := range manifests {
		for mDep := range mani.Dependencies {
			imports = append(imports, pkg.Import{
				Target:   mDep,
				Resolved: IDMap[mDep],
			})
		}
		for mBDep := range mani.BuildDependencies {
			fmt.Println(mBDep)
			imports = append(imports, pkg.Import{
				Target:   mBDep,
				Resolved: IDMap[mBDep],
			})
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transitiveDependenices,
	}, errors.New("stupid")
}
