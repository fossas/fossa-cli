package nuget

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
)

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Logger.Debugf("%#v", a.Module)
	// Parse lockfile.
	lockfile, err := dotnet.ReadLockfile(filepath.Join(Dir(a.Module), "obj", "project.assets.json"))
	if err != nil {
		return graph.Deps{}, err
	}

	// Compute project graph.
	projects := make(map[string]dotnet.Manifest)
	err = Projects(projects, a.Module.BuildTarget)
	if err != nil {
		return graph.Deps{}, err
	}
	root := projects[a.Module.BuildTarget]
	id := pkg.ID{
		Type:     pkg.NuGet,
		Name:     root.Name(),
		Revision: root.Version(),
		Location: a.Module.BuildTarget,
	}

	// Compute package graph.
	deps := make(map[pkg.ID]pkg.Package)
	Packages(projects, lockfile, deps, a.Module.BuildTarget)

	imports := deps[id].Imports
	delete(deps, id)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func Projects(projects map[string]dotnet.Manifest, projectFile string) error {
	// Break out of cycles.
	if _, ok := projects[projectFile]; ok {
		return nil
	}

	// Read manifest.
	var manifest dotnet.Manifest
	err := files.ReadXML(&manifest, projectFile)
	if err != nil {
		return err
	}
	projects[projectFile] = manifest

	// Get all project references and recurse.
	for _, reference := range manifest.Projects() {
		err = Projects(projects, filepath.Join(filepath.Dir(projectFile), dotnet.Path(reference.Include)))
		if err != nil {
			return err
		}
	}

	return nil
}

func Packages(projects map[string]dotnet.Manifest, lockfile dotnet.Lockfile, deps map[pkg.ID]pkg.Package, dep string) pkg.ID {
	log.Logger.Debugf("%#v", dep)
	if project, ok := projects[dotnet.Path(dep)]; ok {
		log.Logger.Debugf("%#v", project)
		name := project.Name()
		version := project.Version()

		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     name,
			Revision: version,
			Location: dep,
		}

		var imports []pkg.Import
		for _, ref := range project.Packages() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: Packages(projects, lockfile, deps, ref.Include),
			})
		}
		for _, ref := range project.Projects() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: Packages(projects, lockfile, deps, filepath.Join(filepath.Dir(dep), dotnet.Path(ref.Include))),
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}

		return id
	} else {
		name := dep
		version := lockfile.Resolve(name)

		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     name,
			Revision: version,
		}

		var imports []pkg.Import
		for p, version := range lockfile.Imports(dep) {
			imports = append(imports, pkg.Import{
				Target:   p + "@" + version,
				Resolved: Packages(projects, lockfile, deps, p),
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}

		return id
	}
}
