package analyzer

import (
	"github.com/fossas/fossa-cli/buildtools/stack"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestAnalyzeStackPure(t *testing.T) {
	stackImmediateDeps := []stack.Dep{
		{
			Name: "package-three",
			Version: "0.0.1.0",
		},
		{
			Name: "package-four",
			Version: "0.0.1.0",
		},
	}

	stackAllDeps := append(stackImmediateDeps,
		stack.Dep{
			Name: "package-one",
			Version: "0.0.1.0",
		},
		stack.Dep{
			Name: "package-two",
			Version: "0.0.1.0",
		},
	)

	depMap := map[stack.Canonical][]stack.Canonical{
		{Identifier: "package-one-0.0.1.0"}: {
			{Identifier: "package-two-0.0.1.0"},
		},
		{Identifier: "package-two-0.0.1.0"}: {
			{Identifier: "package-three-0.0.1.0"},
			{Identifier: "package-four-0.0.1.0"},
		},
		{Identifier: "package-three-0.0.1.0"}: {
			{Identifier: "package-four-0.0.1.0"},
		},
	}

	// Shared model example -- when modifying this, also modify in cabal_test.go
	deps := AnalyzeStackPure(stackAllDeps, stackImmediateDeps, depMap)

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
