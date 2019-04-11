package analyzer

import (
	"errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/mitchellh/mapstructure"
	"path/filepath"
)

const CabalPlanRelPath = "dist-newstyle/cache/plan.json"

type CabalPlan struct {
	InstallPlans []InstallPlan `mapstructure:"install-plan"`
}

type InstallPlan struct {
	Type    string   `mapstructure:"type"`
	Id      string   `mapstructure:"id"`
	Name    string   `mapstructure:"pkg-name"`
	Version string   `mapstructure:"pkg-version"`

	Components map[string]Component `mapstructure:"components"` // Not always present
	Depends    []string             `mapstructure:"depends"`    // Dependencies can be defined here or in Components.*.Depends
	Style      string               `mapstructure:"style"`      // Only exists for packages with type `configured`
}

type Component struct {
	Depends []string `mapstructure:"depends"`
}

func AnalyzeCabal(m module.Module) (graph.Deps, error) {
	cabalPlanPath := filepath.Join(m.Dir, CabalPlanRelPath)

	// If plan.json doesn't exist, generate it
	if exists, _ := files.Exists(cabalPlanPath); !exists {
		_, _, err := exec.Run(exec.Cmd{
			Name: "cabal",
			Argv: []string{"new-build", "--dry-run"},
		})

		if err != nil {
			return graph.Deps{}, err
		}
	}

	if exists, _ := files.Exists(cabalPlanPath); !exists {
		// TODO: fallback to another strategy?
		return graph.Deps{}, errors.New("couldn't find or generate cabal solver plan")
	}

	// Parse cabal new-build's build plan
	var rawPlan map[string]interface{}
	var plan    CabalPlan

	if err := files.ReadJSON(&rawPlan, cabalPlanPath); err != nil {
		return graph.Deps{}, err
	}
	if err := mapstructure.Decode(rawPlan, &plan); err != nil {
		return graph.Deps{}, err
	}

	// Key a map of install plans by their IDs
	// this is used when building out the dependency graph
	var installPlans = make(map[string]InstallPlan)
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

	return deps, nil
}

func InstallPlanToID(plan InstallPlan) pkg.ID {
	return pkg.ID{
		Type:     pkg.Haskell,
		Name:     plan.Name,
		Revision: plan.Version,
	}
}

func InstallPlanToPackage(installPlans map[string]InstallPlan, plan InstallPlan) pkg.Package {
	var imports []pkg.Import

	for _, depId := range plan.Depends {
		dep := installPlans[depId]

		imports = append(imports, pkg.Import{
			Resolved: InstallPlanToID(dep),
		})
	}

	for _, component := range plan.Components {
		for _, depId := range component.Depends {
			dep := installPlans[depId]

			imports = append(imports, pkg.Import{
				Resolved: InstallPlanToID(dep),
			})
		}
	}

	return pkg.Package{
		ID: InstallPlanToID(plan),
		Imports: imports,
	}
}
