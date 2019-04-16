package analyzer

import (
	"github.com/fossas/fossa-cli/buildtools/stack"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func AnalyzeStack(m module.Module) (graph.Deps, error) {
	// Get all deps from
	stackAllDeps, err := stack.GetStackDependencies(m.Dir, nil)

	if err != nil {
		return graph.Deps{}, err
	}

	ghcPkgDeps, err := stack.GetGhcPkgDepMap()

	if err != nil {
		return graph.Deps{}, err
	}

	depth := int(1)
	stackImmediateDeps, err := stack.GetStackDependencies(m.Dir, &depth)

	if err != nil {
		return graph.Deps{}, err
	}

	return AnalyzeStackPure(stackAllDeps, stackImmediateDeps, ghcPkgDeps), nil
}

func AnalyzeStackPure(stackAllDeps []stack.Dep, stackImmediateDeps []stack.Dep, depMap stack.GhcPkgDeps) graph.Deps {
	// Our direct dependencies
	var depGraph = make(map[pkg.ID]pkg.Package)

	// Build out the full graph
	for _, stackDep := range stackAllDeps {
		pkgID := stack.DepToPkgId(stackDep)

		var imports []pkg.Import

		for _, ghcPkgDep := range depMap[stack.DepToCanonical(stackDep)] {
			imports = append(imports, pkg.Import{
				Resolved: stack.DepToPkgId(stack.DepFromCanonical(ghcPkgDep)),
			})
		}

		depGraph[pkgID] = pkg.Package{
			ID: pkgID,
			Imports: imports,
		}
	}

	var directImports []pkg.Import

	// Build our direct dependencies
	for _, stackDep := range stackImmediateDeps {
		pkgID := stack.DepToPkgId(stackDep)

		directImports = append(directImports, pkg.Import{
			Resolved: pkgID,
		})
	}

	return graph.Deps{
		Direct: directImports,
		Transitive: depGraph,
	}
}
