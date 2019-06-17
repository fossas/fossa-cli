package stack_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/stack"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDepFromCanonical(t *testing.T) {
	t.Run("it should form an isomorphism with DepToCanonical", func(t *testing.T) {
		dep := stack.Dep{Name: "package-one", Version: "0.0.1.0"}
		assert.Equal(t, stack.DepFromCanonical(stack.DepToCanonical(dep)), dep)
	})
}

func TestDepToCanonical(t *testing.T) {
	t.Run("it should form an isomorphism with DepFromCanonical", func(t *testing.T) {
		dep := stack.Canonical{Identifier: "package-one-0.0.1.0"}
		assert.Equal(t, stack.DepToCanonical(stack.DepFromCanonical(dep)), dep)
	})
}

func TestDepToPkgId(t *testing.T) {
	dep := stack.Dep{Name: "package-one", Version: "0.0.1.0"}
	result := stack.DepToPkgId(dep)

	assert.Equal(t, result.Name, dep.Name)
	assert.Equal(t, result.Revision, dep.Version)
	assert.Equal(t, result.Type, pkg.Haskell)
}

func TestParseStackDependencies(t *testing.T) {
	packageOne := stack.Dep{Name: "package-one", Version: "0.0.1.0"}
	packageTwo := stack.Dep{Name: "package-two", Version: "0.0.1.0"}
	const packageOneRaw = "package-one 0.0.1.0"
	const packageTwoRaw = "package-two 0.0.1.0"

	joinLines := func(deps ...string) string {
		return strings.Join(deps, "\n")
	}

	t.Run("it should parse a single package", func(t *testing.T) {
		deps := stack.ParseStackDependencies(packageOneRaw)
		assert.ElementsMatch(t, []stack.Dep{packageOne}, deps)
	})
	t.Run("it should parse many packages", func(t *testing.T) {
		deps := stack.ParseStackDependencies(joinLines(packageOneRaw, packageTwoRaw))
		assert.Equal(t, []stack.Dep{packageOne, packageTwo}, deps)
	})
	t.Run("it should ignore empty or invalid lines", func(t *testing.T) {
		deps := stack.ParseStackDependencies("notvalid\n\n")
		assert.Len(t, deps, 0)
	})
}

func TestParseGhcPkgDepMap(t *testing.T) {
	packageOne := stack.Canonical{Identifier: "package-one-0.0.1.0"}
	packageTwo := stack.Canonical{Identifier: "package-two-0.0.1.0"}
	packageThree := stack.Canonical{Identifier: "package-three-0.0.1.0"}
	packageFour := stack.Canonical{Identifier: "package-four-0.0.1.0"}
	const (
		packageOneRaw   = "\"package-one-0.0.1.0\""
		packageTwoRaw   = "\"package-two-0.0.1.0\""
		packageThreeRaw = "\"package-three-0.0.1.0\""
		packageFourRaw  = "\"package-four-0.0.1.0\""
	)

	createDep := func(from string, to string) string {
		return from + " -> " + to
	}

	joinLines := func(deps ...string) string {
		return strings.Join(deps, "\n")
	}

	t.Run("it should parse a single dependency", func(t *testing.T) {
		deps := stack.ParseGhcPkgDepMap(createDep(packageOneRaw, packageTwoRaw))
		assert.Equal(t, deps, map[stack.Canonical][]stack.Canonical{
			packageOne: {packageTwo},
		})
	})
	t.Run("it should parse many dependencies", func(t *testing.T) {
		deps := stack.ParseGhcPkgDepMap(joinLines(createDep(packageOneRaw, packageTwoRaw), createDep(packageThreeRaw, packageFourRaw)))
		assert.Equal(t, deps, map[stack.Canonical][]stack.Canonical{
			packageOne:   {packageTwo},
			packageThree: {packageFour},
		})
	})
	t.Run("it should combine dependencies", func(t *testing.T) {
		deps := stack.ParseGhcPkgDepMap(joinLines(createDep(packageOneRaw, packageTwoRaw), createDep(packageOneRaw, packageThreeRaw)))
		assert.Equal(t, deps, map[stack.Canonical][]stack.Canonical{
			packageOne: {packageTwo, packageThree},
		})
	})
	t.Run("it should ignore empty or invalid lines", func(t *testing.T) {
		deps := stack.ParseGhcPkgDepMap("notvalid\n\n")
		assert.Len(t, deps, 0)
	})
}

func TestAnalyzeStackPure(t *testing.T) {
	stackImmediateDeps := []stack.Dep{
		{
			Name:    "package-three",
			Version: "0.0.1.0",
		},
		{
			Name:    "package-four",
			Version: "0.0.1.0",
		},
	}

	stackAllDeps := append(stackImmediateDeps,
		stack.Dep{
			Name:    "package-one",
			Version: "0.0.1.0",
		},
		stack.Dep{
			Name:    "package-two",
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
	deps := stack.GetDepsPure(stackAllDeps, stackImmediateDeps, depMap)

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
