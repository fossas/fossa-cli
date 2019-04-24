package dotnet

import (
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type DotNET struct {
	Cmd     string
	Version string
}

func (d *DotNET) Build(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: d.Cmd,
		Argv: []string{"restore"},
		Dir:  dir,
	})
	return err
}

func Path(s string) string {
	return filepath.Join(strings.Split(s, "\\")...)
}

func ResolveStrategy(target, dir string) (graph.Deps, error) {
	// Parse lockfile.
	lockfile, err := readLockfile(filepath.Join(dir, "obj", "project.assets.json"))
	if err != nil {
		return graph.Deps{}, err
	}

	// Compute project graph.
	projects := make(map[string]Manifest)
	err = Projects(projects, target)
	if err != nil {
		return graph.Deps{}, err
	}
	root := projects[target]
	id := pkg.ID{
		Type:     pkg.NuGet,
		Name:     root.Name(),
		Revision: root.Version(),
		Location: target,
	}

	// Compute package graph.
	deps := make(map[pkg.ID]pkg.Package)
	packagesOutput(projects, lockfile, deps, target)

	imports := deps[id].Imports
	delete(deps, id)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil

}

func packagesOutput(projects map[string]Manifest, lockfile Lockfile, deps map[pkg.ID]pkg.Package, dep string) pkg.ID {
	if project, ok := projects[Path(dep)]; ok {
		log.Debugf("%#v", project)
		name := project.Name()
		version := project.Version()

		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     name,
			Revision: version,
			Location: dep,
		}

		var imports []pkg.Import
		for _, ref := range project.packages() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: packagesOutput(projects, lockfile, deps, ref.Include),
			})
		}
		for _, ref := range project.projects() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: packagesOutput(projects, lockfile, deps, filepath.Join(filepath.Dir(dep), Path(ref.Include))),
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}

		return id
	} else {
		name := dep
		version := lockfile.resolve(name)

		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     name,
			Revision: version,
		}

		var imports []pkg.Import
		for p, version := range lockfile.imports(dep) {
			imports = append(imports, pkg.Import{
				Target:   p + "@" + version,
				Resolved: packagesOutput(projects, lockfile, deps, p),
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}

		return id
	}
}
