package dotnet

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Project struct {
	Version      string                 `json:"version"`
	Dependencies map[string]interface{} `json:"dependencies"`
}

func ProjectGraph(target string) (graph.Deps, error) {
	var project Project
	err := files.ReadJSON(&project, target)
	if err != nil {
		return graph.Deps{}, err
	}

	depMap := make(map[pkg.ID]pkg.Package)
	imports := []pkg.Import{}

	for name, fields := range project.Dependencies {
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
