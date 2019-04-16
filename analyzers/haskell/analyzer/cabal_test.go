package analyzer

import (
	"github.com/fossas/fossa-cli/buildtools/cabal"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestAnalyzeCabalPure(t *testing.T) {
	// Basic test with an input that hits all of the edge-cases:
	// - pre-existing libraries
	// - dependencies within components
	// - dependencies at the top level
	input := cabal.Plan{
		InstallPlans: []cabal.InstallPlan{
			{
				Type: "configured",
				Id: "pckg-n-0.0.1.0",
				Name: "package-one",
				Version: "0.0.1.0",

				Components: map[string]cabal.Component{
					"lib": {
						Depends: []string{"pckg-tw-0.0.1.0"},
					},
				},
				Style: "global",
			},
			{
				Type: "configured",
				Id: "pckg-tw-0.0.1.0",
				Name: "package-two",
				Version: "0.0.1.0",

				Depends: []string{"pkcg-thr-0.0.1.0", "pkcg-fr-0.0.1.0"},
				Style: "local",
			},
			{
				Type: "pre-existing",
				Id: "pkcg-thr-0.0.1.0",
				Name: "package-three",
				Version: "0.0.1.0",

				Depends: []string{"pkcg-fr-0.0.1.0"},
			},
			{
				Type: "configured",
				Id: "pkcg-fr-0.0.1.0",
				Name: "package-four",
				Version: "0.0.1.0",

				Depends: []string{},
				Style: "global",
			},
		},
	}

	deps := AnalyzeCabalPure(input)

	packageOne := pkg.ID{
		Type: pkg.Haskell,
		Name: "package-one",
		Revision: "0.0.1.0",
	}
	packageTwo := pkg.ID{
		Type: pkg.Haskell,
		Name: "package-two",
		Revision: "0.0.1.0",
	}
	packageThree := pkg.ID{
		Type: pkg.Haskell,
		Name: "package-three",
		Revision: "0.0.1.0",
	}
	packageFour := pkg.ID{
		Type: pkg.Haskell,
		Name: "package-four",
		Revision: "0.0.1.0",
	}

	assert.Equal(t, graph.Deps{
		Direct: []pkg.Import{
			{
				Resolved: packageThree,
			},
			{
				Resolved: packageFour,
			},
		},
		Transitive: map[pkg.ID]pkg.Package{
			packageOne: {
				ID: packageOne,
				Imports: []pkg.Import{
					{
						Resolved: packageTwo,
					},
				},
			},
			packageTwo: {
				ID: packageTwo,
				Imports: []pkg.Import{
					{
						Resolved: packageThree,
					},
					{
						Resolved: packageFour,
					},
				},
			},
			packageThree: {
				ID: packageThree,
				Imports: []pkg.Import{
					{
						Resolved: packageFour,
					},
				},
			},
			packageFour: {
				ID: packageFour,
				Imports: nil,
			},
		},
	}, deps)
}
