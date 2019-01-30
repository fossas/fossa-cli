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

// Spec defines a paket dependency and all of its attributes.
type Spec struct {
	Remote       string
	Name         string
	Version      string
	Type         pkg.Type
	Dependencies []string
}

// DependencyGraph produces a full dep graph with access to a "paket.lock" file.
func DependencyGraph(target string) (graph.Deps, error) {
	groups, err := readLockfile(target)
	if err != nil {
		return graph.Deps{}, err
	}

	depGraph, err := graphFromLockfile(groups)
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

	lockfile := []Group{}
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
		lockfile = append(lockfile, appendedGroup)
	}

	return lockfile, nil
}

func graphFromLockfile(lockfile []Group) (graph.Deps, error) {
	packageMap := make(map[string]pkg.Import)
	for _, group := range lockfile {
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
	for _, group := range lockfile {
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
