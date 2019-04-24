package dotnet

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Packages struct {
	Package []Package `xml:"package"`
}

type Package struct {
	ID      string `xml:"id,attr"`
	Version string `xml:"version,attr"`
}

// PackageConfigGraph reads a `packages.config` file and returns a dependency graph.
func PackageConfigGraph(file string) (graph.Deps, error) {
	var pacconfig Packages
	err := files.ReadXML(&pacconfig, file)
	if err != nil {
		return graph.Deps{}, err
	}

	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}
	for _, dep := range pacconfig.Package {
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
