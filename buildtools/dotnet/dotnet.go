package dotnet

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type DotNET struct {
	Cmd string
}

func (d *DotNET) Build(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: d.Cmd,
		Argv: []string{"restore"},
		Dir:  dir,
	})
	return err
}

func (d *DotNET) References(projectFile string) ([]string, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: d.Cmd,
		Argv: []string{"list", projectFile, "reference"},
		WithEnv: map[string]string{
			"TERM": "dumb",
		},
	})
	if err != nil {
		return nil, err
	}

	header := "Project reference(s)\n--------------------\n"
	headerIndex := strings.Index(stdout, header)
	if headerIndex == -1 {
		return nil, nil
	}

	var projects []string
	contents := stdout[headerIndex+len(header):]
	for _, line := range strings.Split(contents, "\n") {
		if line == "" {
			break
		}
		project := filepath.Join(filepath.Dir(projectFile), Path(line))
		projects = append(projects, project)
	}

	return projects, nil
}

func Path(s string) string {
	return filepath.Join(strings.Split(s, "\\")...)
}

// PackageReferenceGraph reads a package reference file and returns a dependency graph.
func PackageReferenceGraph(file string) (graph.Deps, error) {
	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}
	projects := make(map[string]Manifest)
	Projects(projects, file)
	for _, man := range projects {
		fmt.Println(man.Name())
		for _, packages := range man.Packages() {
			id := pkg.ID{
				Type:     pkg.NuGet,
				Name:     packages.Include,
				Revision: packages.Version,
			}
			imports = append(imports, pkg.Import{
				Target:   packages.Include,
				Resolved: id,
			})
			depMap[id] = pkg.Package{
				ID: id,
			}
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: depMap,
	}, nil
}

func NuspecGraph(file string) (graph.Deps, error) {
	var nuspec NuSpec
	err := files.ReadXML(&nuspec, file)
	if err != nil {
		return graph.Deps{}, err
	}

	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}
	for _, group := range nuspec.Metadata.Dependencies.Groups {
		for _, dep := range group.Dependencies {
			id := pkg.ID{
				Type:     pkg.NuGet,
				Name:     dep.ID,
				Revision: dep.Version,
			}
			imports = append(imports, pkg.Import{
				Target:   dep.ID,
				Resolved: id,
			})
			depMap[id] = pkg.Package{
				ID: id,
			}
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: depMap,
	}, nil
}

func PackageConfigGraph(file string) (graph.Deps, error) {
	var pacconfig Packages
	err := files.ReadXML(&pacconfig, file)
	if err != nil {
		return graph.Deps{}, err
	}

	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}
	for _, dep := range pacconfig.Package {
		fmt.Println(dep.ID, dep.Version)
		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     dep.ID,
			Revision: dep.Version,
		}
		imports = append(imports, pkg.Import{
			Target:   dep.ID,
			Resolved: id,
		})
		depMap[id] = pkg.Package{
			ID: id,
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: depMap,
	}, nil
}

func ProjectGraph(target string) (graph.Deps, error) {
	var project2 Project
	err := files.ReadJSON(&project2, target)
	if err != nil {
		fmt.Println("error reading", err)
		return graph.Deps{}, err
	}

	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}

	for name, fields := range project2.Dependencies {
		version := ""
		switch v := fields.(type) {
		case string:
			version = v
		case map[string]interface{}:
			mapVersion, isString := v["version"].(string)
			if isString {
				version = mapVersion
			}
		}

		id := pkg.ID{
			Type:     pkg.NuGet,
			Name:     name,
			Revision: version,
		}
		imports = append(imports, pkg.Import{Target: name, Resolved: id})
		depMap[id] = pkg.Package{ID: id}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: depMap,
	}, nil
}

func ResolveStrategy(target, dir string) (graph.Deps, error) {
	// Parse lockfile.
	lockfile, err := ReadLockfile(filepath.Join(dir, "obj", "project.assets.json"))
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
	PackagesOutput(projects, lockfile, deps, target)

	imports := deps[id].Imports
	delete(deps, id)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil

}

func Projects(projects map[string]Manifest, projectFile string) error {
	// Break out of cycles.
	if _, ok := projects[projectFile]; ok {
		return nil
	}

	// Read manifest.
	var manifest Manifest
	err := files.ReadXML(&manifest, projectFile)
	if err != nil {
		return err
	}
	projects[projectFile] = manifest

	// Get all project references and recurse.
	for _, reference := range manifest.Projects() {
		err = Projects(projects, filepath.Join(filepath.Dir(projectFile), Path(reference.Include)))
		if err != nil {
			return err
		}
	}

	return nil
}

func PackagesOutput(projects map[string]Manifest, lockfile Lockfile, deps map[pkg.ID]pkg.Package, dep string) pkg.ID {
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
		for _, ref := range project.Packages() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: PackagesOutput(projects, lockfile, deps, ref.Include),
			})
		}
		for _, ref := range project.Projects() {
			imports = append(imports, pkg.Import{
				Target:   ref.Include + "@" + ref.Version,
				Resolved: PackagesOutput(projects, lockfile, deps, filepath.Join(filepath.Dir(dep), Path(ref.Include))),
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
				Resolved: PackagesOutput(projects, lockfile, deps, p),
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}

		return id
	}
}
