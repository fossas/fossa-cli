package python

import (
	"github.com/fossas/fossa-cli/buildtools/pip"
	"github.com/fossas/fossa-cli/pkg"
)

func FromRequirements(reqs []pip.Requirement) []pkg.Import {
	var imports []pkg.Import
	for _, req := range reqs {
		imports = append(imports, pkg.Import{
			Target: req.String(),
			Resolved: pkg.ID{
				Type: pkg.Python,
				Name: req.Name,
				// TODO: the backend isn't equipped to handle version ranges
				Revision: req.Constraints[0].Revision,
			},
		})
	}
	return imports
}

func FromTree(tree []pip.DepTree) ([]pkg.Import, map[pkg.ID]pkg.Package) {
	var imports []pkg.Import
	for _, dep := range tree {
		imports = append(imports, pkg.Import{
			Target: dep.Target,
			Resolved: pkg.ID{
				Type:     pkg.Python,
				Name:     dep.Package,
				Revision: dep.Resolved,
			},
		})
	}

	graph := make(map[pkg.ID]pkg.Package)
	for _, subtree := range tree {
		flattenTree(graph, subtree)
	}

	return imports, graph
}

func flattenTree(graph map[pkg.ID]pkg.Package, tree pip.DepTree) {
	for _, dep := range tree.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.Python,
			Name:     dep.Package,
			Revision: dep.Resolved,
		}
		// Don't process duplicates.
		_, ok := graph[id]
		if ok {
			continue
		}
		// Get direct imports.
		var imports []pkg.Import
		for _, i := range dep.Dependencies {
			imports = append(imports, pkg.Import{
				Resolved: pkg.ID{
					Type:     pkg.Python,
					Name:     i.Package,
					Revision: i.Resolved,
				},
			})
		}
		// Update map.
		graph[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		// Recurse in imports.
		flattenTree(graph, dep)
	}
}
