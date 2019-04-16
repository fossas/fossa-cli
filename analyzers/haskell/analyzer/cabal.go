package analyzer

import (
	"github.com/fossas/fossa-cli/buildtools/cabal"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func AnalyzeCabal(m module.Module) (graph.Deps, error) {
	plan, err := cabal.GetSolverPlan(m.Dir)

	if err != nil {
		return graph.Deps{}, err
	}

	return AnalyzeCabalPure(plan), nil
}

func AnalyzeCabalPure(plan cabal.Plan) graph.Deps {
	// Key a map of install plans by their IDs
	// this is used when building out the dependency graph
	var installPlans = make(map[string]cabal.InstallPlan)
	for _, p := range plan.InstallPlans {
		installPlans[p.Id] = p
	}

	var dependencyGraph = make(map[pkg.ID]pkg.Package)
	var directDependencies []pkg.Import

	// Build the entire dependency graph, keeping track of our direct
	// dependencies
	for _, p := range plan.InstallPlans {
		var builtPackage = InstallPlanToPackage(installPlans, p)

		if p.Type == "configured" && p.Style == "local" {
			directDependencies = append(directDependencies, builtPackage.Imports...)
		}

		dependencyGraph[builtPackage.ID] = builtPackage
	}

	deps := graph.Deps{
		Direct: directDependencies,
		Transitive: dependencyGraph,
	}

	return deps
}

func InstallPlanToPackage(installPlans map[string]cabal.InstallPlan, plan cabal.InstallPlan) pkg.Package {
	var imports []pkg.Import

	for _, depId := range plan.Depends {
		dep := installPlans[depId]

		imports = append(imports, pkg.Import{
			Resolved: cabal.InstallPlanToID(dep),
		})
	}

	for _, component := range plan.Components {
		for _, depId := range component.Depends {
			dep := installPlans[depId]

			imports = append(imports, pkg.Import{
				Resolved: cabal.InstallPlanToID(dep),
			})
		}
	}

	return pkg.Package{
		ID: cabal.InstallPlanToID(plan),
		Imports: imports,
	}
}
