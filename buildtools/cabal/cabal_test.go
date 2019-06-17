package cabal_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/cabal"
	"github.com/fossas/fossa-cli/pkg"
)

func TestAnalyzeCabalPure(t *testing.T) {
	// Basic test with an input that hits all of the edge-cases:
	// - pre-existing libraries
	// - dependencies within components
	// - dependencies at the top level
	input := cabal.Plan{
		InstallPlans: []cabal.InstallPlan{
			{
				Type:    "configured",
				Id:      "pckg-n-0.0.1.0",
				Name:    "package-one",
				Version: "0.0.1.0",

				Components: map[string]cabal.Component{
					"lib": {
						Depends: []string{"pckg-tw-0.0.1.0"},
					},
				},
				Style: "global",
			},
			{
				Type:    "configured",
				Id:      "pckg-tw-0.0.1.0",
				Name:    "package-two",
				Version: "0.0.1.0",

				Depends: []string{"pkcg-thr-0.0.1.0", "pkcg-fr-0.0.1.0"},
				Style:   "local",
			},
			{
				Type:    "pre-existing",
				Id:      "pkcg-thr-0.0.1.0",
				Name:    "package-three",
				Version: "0.0.1.0",

				Depends: []string{"pkcg-fr-0.0.1.0"},
			},
			{
				Type:    "configured",
				Id:      "pkcg-fr-0.0.1.0",
				Name:    "package-four",
				Version: "0.0.1.0",

				Depends: []string{},
				Style:   "global",
			},
		},
	}

	// Shared model example -- when modifying this, also modify in stack_test.go
	deps := cabal.GetDepsPure(input)

	packageOne := pkg.ID{
		Type:     pkg.Haskell,
		Name:     "package-one",
		Revision: "0.0.1.0",
	}
	packageTwo := pkg.ID{
		Type:     pkg.Haskell,
		Name:     "package-two",
		Revision: "0.0.1.0",
	}
	packageThree := pkg.ID{
		Type:     pkg.Haskell,
		Name:     "package-three",
		Revision: "0.0.1.0",
	}
	packageFour := pkg.ID{
		Type:     pkg.Haskell,
		Name:     "package-four",
		Revision: "0.0.1.0",
	}

	assert.Len(t, deps.Direct, 2)
	assert.Contains(t, deps.Direct, pkg.Import{Resolved: packageThree})
	assert.Contains(t, deps.Direct, pkg.Import{Resolved: packageFour})

	assert.Len(t, deps.Transitive, 4)

	assert.Contains(t, deps.Transitive, packageOne)
	assert.Len(t, deps.Transitive[packageOne].Imports, 1)
	assert.Contains(t, deps.Transitive[packageOne].Imports, pkg.Import{Resolved: packageTwo})

	assert.Contains(t, deps.Transitive, packageTwo)
	assert.Len(t, deps.Transitive[packageTwo].Imports, 2)
	assert.Contains(t, deps.Transitive[packageTwo].Imports, pkg.Import{Resolved: packageThree})
	assert.Contains(t, deps.Transitive[packageTwo].Imports, pkg.Import{Resolved: packageFour})

	assert.Contains(t, deps.Transitive, packageThree)
	assert.Len(t, deps.Transitive[packageThree].Imports, 1)
	assert.Contains(t, deps.Transitive[packageThree].Imports, pkg.Import{Resolved: packageFour})

	assert.Contains(t, deps.Transitive, packageFour)
	assert.Len(t, deps.Transitive[packageFour].Imports, 0)
}
