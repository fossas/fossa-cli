package ruby

import (
	"github.com/fossas/fossa-cli/buildtools/bundler"
	"github.com/fossas/fossa-cli/pkg"
)

func FromGems(gems []bundler.Gem) ([]pkg.Import, map[pkg.ID]pkg.Package) {
	var imports []pkg.Import
	graph := make(map[pkg.ID]pkg.Package)
	for _, gem := range gems {
		id := pkg.ID{
			Type:     pkg.Ruby,
			Name:     gem.Name,
			Revision: gem.Revision,
		}
		imports = append(imports, pkg.Import{
			Resolved: id,
		})
		graph[id] = pkg.Package{
			ID:       id,
			Strategy: "bundler-list",
		}
	}
	return imports, graph
}

func FromLockfile(lockfile bundler.Lockfile) ([]pkg.Import, map[pkg.ID]pkg.Package) {
	// Construct a map of all dependencies.
	nameToID := make(map[string]pkg.ID)

	AddSpecs(nameToID, lockfile.Git)
	AddSpecs(nameToID, lockfile.Path)
	AddSpecs(nameToID, lockfile.Gem)

	// Build the dependency graph.
	graph := make(map[pkg.ID]pkg.Package)

	AddToGraph(graph, nameToID, lockfile.Git)
	AddToGraph(graph, nameToID, lockfile.Path)
	AddToGraph(graph, nameToID, lockfile.Gem)

	var imports []pkg.Import
	for _, dep := range lockfile.Dependencies {
		imports = append(imports, pkg.Import{
			Target:   dep.String(),
			Resolved: nameToID[dep.Name],
		})
	}

	return imports, graph
}

func AddSpecs(lookup map[string]pkg.ID, sections []bundler.Section) {
	for _, section := range sections {
		for _, spec := range section.Specs {
			location := section.Remote
			if section.Type == "GIT" {
				location = section.Remote + "@" + section.Revision
			}
			lookup[spec.Name] = pkg.ID{
				Type:     pkg.Ruby,
				Name:     spec.Name,
				Revision: spec.Version,
				Location: location,
			}
		}
	}
}

func AddToGraph(graph map[pkg.ID]pkg.Package, lookup map[string]pkg.ID, sections []bundler.Section) {
	for _, section := range sections {
		for _, spec := range section.Specs {
			var imports []pkg.Import
			for _, dep := range spec.Dependencies {
				imports = append(imports, pkg.Import{
					Target:   dep.String(),
					Resolved: lookup[dep.Name],
				})
			}

			id := lookup[spec.Name]
			graph[id] = pkg.Package{
				ID:      id,
				Imports: imports,
			}
		}
	}
}

func FilteredLockfile(gems []bundler.Gem, lockfile bundler.Lockfile) ([]pkg.Import, map[pkg.ID]pkg.Package) {
	// Construct set of allowed gems.
	gemSet := make(map[string]bool)
	for _, gem := range gems {
		gemSet[gem.Name] = true
	}

	// Filter lockfile results.
	imports, graph := FromLockfile(lockfile)

	var filteredImports []pkg.Import
	filteredGraph := make(map[pkg.ID]pkg.Package)

	for _, dep := range imports {
		if _, ok := gemSet[dep.Resolved.Name]; ok {
			filteredImports = append(filteredImports, dep)
		}
	}

	for id, pkg := range graph {
		if _, ok := gemSet[id.Name]; ok {
			filteredGraph[id] = pkg
		}
	}

	return filteredImports, filteredGraph
}
