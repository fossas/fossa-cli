package cabal

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
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
	Type    string   `mapstructure:"type"`
	Id      string   `mapstructure:"id"`
	Name    string   `mapstructure:"pkg-name"`
	Version string   `mapstructure:"pkg-version"`

	Components map[string]Component `mapstructure:"components"` // Not always present
	Depends    []string             `mapstructure:"depends"`    // Dependencies can be defined here or in Components.*.Depends
	Style      string               `mapstructure:"style"`      // Only exists for packages with type `configured`
}

func InstallPlanToID(plan InstallPlan) pkg.ID {
	return pkg.ID{
		Type:     pkg.Haskell,
		Name:     plan.Name,
		Revision: plan.Version,
	}
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
			Argv: []string{"new-build", "--dry-run"},
			Dir: dir,
		})

		if err != nil {
			return Plan{}, err
		}
	}

	if exists, _ := files.Exists(cabalPlanPath); !exists {
		// TODO: fallback to another strategy?
		return Plan{}, errors.New("couldn't find or generate cabal solver plan")
	}

	// Parse cabal new-build's build plan
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
