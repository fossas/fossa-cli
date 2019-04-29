package dotnet

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type NuSpec struct {
	Metadata Metadata `xml:"metadata"`
}

type Metadata struct {
	ID           string       `xml:"id"`
	Version      string       `xml:"version"`
	Dependencies Dependencies `xml:"dependencies"`
}

type Dependencies struct {
	Groups []Group `xml:"group"`
}

type Group struct {
	Dependencies []Dependency `xml:"dependency"`
}

type Dependency struct {
	ID      string `xml:"id,attr"`
	Version string `xml:"version,attr"`
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
