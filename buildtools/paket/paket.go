package paket

import (
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Group defines a paket dependency group.
type Group struct {
	Name  string
	Specs []Spec
}

// Spec defines a paket dependency and its attributes.
type Spec struct {
	Remote       string
	Name         string
	Version      string
	Type         pkg.Type
	Dependencies []string
}

// DependencyGraph parses a "paket.lock" file to create a dependency graph.
func DependencyGraph(target string) (graph.Deps, error) {
	groups, err := readLockfile(target)
	if err != nil {
		return graph.Deps{}, err
	}

	depGraph, err := graphFromGroups(groups)
	if err != nil {
		return graph.Deps{}, err
	}

	return depGraph, nil
}

func readLockfile(filename string) ([]Group, error) {
	contents, err := files.Read(filename)
	if err != nil {
		return []Group{}, errors.Wrapf(err, "could not read lockfile: %s", filename)
	}

	groups := []Group{}
	groupSplit := strings.Split(string(contents), "GROUP ")
	for _, group := range groupSplit {
		newSpec := Spec{}
		newGroup := Group{}
		packagetype := pkg.NuGet
		remote := ""

		sections := strings.Split(group, "\n")
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
				if !strings.HasPrefix(section, "  ") {
					continue
				}

				if strings.Contains(section, "remote:") {
					remote = strings.Split(section, "remote: ")[1]
					continue
				}

				matches := strings.Split(section, " ")
				if strings.HasPrefix(section, "      ") {
					if len(matches) >= 7 {
						newSpec.Dependencies = append(newSpec.Dependencies, matches[6])
					}
					continue
				}

				if newSpec.Name != "" {
					newGroup.Specs = append(newGroup.Specs, newSpec)
				}

				if len(matches) >= 6 {
					revision := findRevision(matches[5])
					newSpec = Spec{
						Remote:  remote,
						Type:    packagetype,
						Name:    matches[4],
						Version: revision,
					}
				}
			}
		}

		if newSpec.Name != "" {
			newGroup.Specs = append(newGroup.Specs, newSpec)
		}
		newGroup.Name = sections[0]
		groups = append(groups, newGroup)
	}
	return groups, nil
}

func graphFromGroups(groups []Group) (graph.Deps, error) {
	packageMap := make(map[string]pkg.Import)
	for _, group := range groups {
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
	for _, group := range groups {
		for _, spec := range group.Specs {
			packageImport := packageMap[spec.Name]
			packageGraph.Direct = append(packageGraph.Direct, packageImport)

			imports := []pkg.Import{}
			for _, dep := range spec.Dependencies {
				imports = append(imports, packageMap[dep])
			}

			packageGraph.Transitive[packageImport.Resolved] = pkg.Package{
				ID:      packageImport.Resolved,
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
