package paket

import (
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Lockfile struct {
	Groups []Group
}
type Group struct {
	Name  string
	Specs []Spec
}

type Spec struct {
	Remote       string
	Name         string
	Version      string
	Type         pkg.Type
	Dependencies []string
}

func DependencyGraph(target string) (graph.Deps, error) {
	// ideally this is a mapping of map[Name]Dependency where Dependency is
	// Name, Revision, and Imports(string), and Type
	lockfile, _ := readLockfile(target)
	graph, _ := graphFromLockfile(lockfile)

	return graph, nil
}

func readLockfile(filename string) (Lockfile, error) {
	contents, err := files.Read(filename)
	if err != nil {
		return Lockfile{}, errors.Wrapf(err, "could not read lockfile: %s", filename)
	}
	lockfile := Lockfile{}
	groups := strings.Split(string(contents), "GROUP ")
	for _, group := range groups {
		newSpec := Spec{}
		appendedGroup := Group{}
		sections := strings.Split(group, "\n")
		packagetype := pkg.NuGet
		remote := ""
		for _, section := range sections {
			switch section {
			case "HTTP":
				fallthrough
			case "GIT":
				fallthrough
			case "GITHUB":
				packagetype = pkg.Git
			case "NUGET":
				packagetype = pkg.NuGet
			default:
				// THIS IS GARBAGE.
				if !strings.HasPrefix(section, "  ") {
					continue
				}
				// Improve this match. Contains would be more robust but not as quick, which doesn't realllly matter.
				if strings.HasPrefix(section, "  remote: ") {
					remote = strings.Split(section, "remote: ")[1]
					continue
				}
				matches := strings.Split(section, " ")
				if strings.HasPrefix(section, "      ") {
					newSpec.Dependencies = append(newSpec.Dependencies, matches[6])
					continue
				}

				if newSpec.Name != "" {
					appendedGroup.Specs = append(appendedGroup.Specs, newSpec)
				}
				revision := findRevision(matches[5])
				newSpec = Spec{
					Remote:  remote,
					Type:    packagetype,
					Name:    matches[4],
					Version: revision,
				}
			}
		}
		if newSpec.Name != "" {
			appendedGroup.Specs = append(appendedGroup.Specs, newSpec)
		}
		appendedGroup.Name = sections[0]
		lockfile.Groups = append(lockfile.Groups, appendedGroup)
	}

	return lockfile, nil
}

func graphFromLockfile(lockfile Lockfile) (graph.Deps, error) {
	packageMap := make(map[string]pkg.Import)
	for _, group := range lockfile.Groups {
		for _, spec := range group.Specs {
			name := spec.Name
			if spec.Type == pkg.Git {
				name = spec.Remote + "/" + spec.Name
			}
			packageMap[spec.Name] = pkg.Import{
				Target: spec.Name,
				Resolved: pkg.ID{
					Type:     spec.Type,
					Name:     name,
					Revision: spec.Version,
					Location: spec.Remote,
				},
			}
		}
	}

	packageGraph := graph.Deps{
		Direct:     []pkg.Import{},
		Transitive: make(map[pkg.ID]pkg.Package),
	}
	for _, group := range lockfile.Groups {
		for _, spec := range group.Specs {
			packID := packageMap[spec.Name].Resolved
			packageGraph.Direct = append(packageGraph.Direct, packageMap[spec.Name])
			imports := []pkg.Import{}
			for _, dep := range spec.Dependencies {
				imports = append(imports, packageMap[dep])
			}
			packageGraph.Transitive[packID] = pkg.Package{
				ID:      packID,
				Imports: imports,
			}
		}
	}
	return packageGraph, nil
}

func findRevision(line string) string {
	revision := strings.TrimPrefix(line, "(")
	revision = strings.TrimSuffix(revision, ")")
	return revision
}
