package cabal

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/mitchellh/mapstructure"
	"path/filepath"
)

const PlanRelPath = "dist-newstyle/cache/plan.json"

// ----- Types

type Plan struct {
	InstallPlans []InstallPlan `mapstructure:"install-plan"`
}

type InstallPlan struct {
	// There are two types of install plans: `pre-existing` and `configured`.
	// `pre-existing` corresponds to system-level libraries, and `configured`
	// is for globally-installed and project-local dependencies. To
	// differentiate, the `style` field is used.
	Type    string `mapstructure:"type"`
	Id      string `mapstructure:"id"`
	Name    string `mapstructure:"pkg-name"`
	Version string `mapstructure:"pkg-version"`

	Components map[string]Component `mapstructure:"components"` // Not always present
	Depends    []string             `mapstructure:"depends"`    // Dependencies can be defined here or in Components.*.Depends
	// This field only exists for packages with Type `configured`. It can
	// contain one of two values: `global` for globally-installed dependencies,
	// or `local` for project-local dependencies
	Style string `mapstructure:"style"` // Only exists for packages with type `configured`
}

func InstallPlanToID(plan InstallPlan) pkg.ID {
	return pkg.ID{
		Type:     pkg.Haskell,
		Name:     plan.Name,
		Revision: plan.Version,
	}
}

func IsDirectDependency(plan InstallPlan) bool {
	// See documentation on InstallPlan
	return plan.Type == "configured" && plan.Style == "local"
}

type Component struct {
	Depends []string `mapstructure:"depends"`
}

// ----- Command invocation

func GetSolverPlan(dir string) (Plan, error) {
	cabalPlanPath := filepath.Join(dir, PlanRelPath)

	// If plan.json doesn't exist, generate it
	if exists, _ := files.Exists(cabalPlanPath); !exists {
		_, _, err := exec.Run(exec.Cmd{
			Name: "cabal",
			Argv: []string{"v2-build", "--dry-run"},
			Dir:  dir,
		})
		if err != nil {
			return Plan{}, err
		}
	}

	if exists, _ := files.Exists(cabalPlanPath); !exists {
		// TODO: fallback to another strategy?
		return Plan{}, errors.New("couldn't find or generate cabal solver plan")
	}

	// Parse cabal v2-build's build plan
	var rawPlan map[string]interface{}
	var plan Plan

	if err := files.ReadJSON(&rawPlan, cabalPlanPath); err != nil {
		return Plan{}, err
	}
	if err := mapstructure.Decode(rawPlan, &plan); err != nil {
		return Plan{}, err
	}

	return plan, nil

}

// TODO --------------------------

func GetDeps(m module.Module) (graph.Deps, error) {
	plan, err := GetSolverPlan(m.Dir)
	if err != nil {
		return graph.Deps{}, err
	}

	return GetDepsPure(plan), nil
}

func GetDepsPure(plan Plan) graph.Deps {
	// Index a list of install plans by their IDs
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

		if IsDirectDependency(p) {
			directDependencies = append(directDependencies, builtPackage.Imports...)
		}

		dependencyGraph[builtPackage.ID] = builtPackage
	}

	deps := graph.Deps{
		Direct:     directDependencies,
		Transitive: dependencyGraph,
	}

	return deps
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
		ID:      InstallPlanToID(plan),
		Imports: imports,
	}
}
