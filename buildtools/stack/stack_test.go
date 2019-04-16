package stack

import (
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestDepFromCanonical(t *testing.T) {
	t.Run("it should form an isomorphism with DepToCanonical", func(t *testing.T) {
		dep := Dep{Name: "package-one", Version: "0.0.1.0"}
		assert.Equal(t, DepFromCanonical(DepToCanonical(dep)), dep)
	})
}

func TestDepToCanonical(t *testing.T) {
	t.Run("it should form an isomorphism with DepFromCanonical", func(t *testing.T) {
		dep := Canonical{Identifier: "package-one-0.0.1.0"}
		assert.Equal(t, DepToCanonical(DepFromCanonical(dep)), dep)
	})
}

func TestDepToPkgId(t *testing.T) {
	dep := Dep{Name: "package-one", Version: "0.0.1.0"}
	result := DepToPkgId(dep)

	assert.Equal(t, result.Name, dep.Name)
	assert.Equal(t, result.Revision, dep.Version)
	assert.Equal(t, result.Type, pkg.Haskell)
}

func TestParseStackDependencies(t *testing.T) {
	packageOne := Dep{Name: "package-one", Version: "0.0.1.0"}
	packageTwo := Dep{Name: "package-two", Version: "0.0.1.0"}
	const packageOneRaw = "package-one 0.0.1.0"
	const packageTwoRaw = "package-two 0.0.1.0"

	joinLines := func(deps ...string) string {
		return strings.Join(deps, "\n")
	}

	t.Run("it should parse a single package", func(t *testing.T) {
		deps := ParseStackDependencies(packageOneRaw)
		assert.ElementsMatch(t, []Dep{packageOne}, deps)
	})
	t.Run("it should parse many packages", func(t *testing.T) {
		deps := ParseStackDependencies(joinLines(packageOneRaw, packageTwoRaw))
		assert.Equal(t, []Dep{packageOne, packageTwo}, deps)
	})
	t.Run("it should ignore empty or invalid lines", func(t *testing.T) {
		deps := ParseStackDependencies("notvalid\n\n")
		assert.Len(t, deps, 0)
	})
}

func TestParseGhcPkgDepMap(t *testing.T) {
	packageOne   := Canonical{Identifier:"package-one-0.0.1.0"}
	packageTwo   := Canonical{Identifier:"package-two-0.0.1.0"}
	packageThree := Canonical{Identifier:"package-three-0.0.1.0"}
	packageFour  := Canonical{Identifier:"package-four-0.0.1.0"}
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
		deps := ParseGhcPkgDepMap(createDep(packageOneRaw, packageTwoRaw))
		assert.Equal(t, deps, map[Canonical][]Canonical{
			packageOne: {packageTwo},
		})
	})
	t.Run("it should parse many dependencies", func(t *testing.T) {
		deps := ParseGhcPkgDepMap(joinLines(createDep(packageOneRaw, packageTwoRaw), createDep(packageThreeRaw, packageFourRaw)))
		assert.Equal(t, deps, map[Canonical][]Canonical{
			packageOne: {packageTwo},
			packageThree: {packageFour},
		})
	})
	t.Run("it should combine dependencies", func(t *testing.T) {
		deps := ParseGhcPkgDepMap(joinLines(createDep(packageOneRaw, packageTwoRaw), createDep(packageOneRaw, packageThreeRaw)))
		assert.Equal(t, deps, map[Canonical][]Canonical{
			packageOne: {packageTwo, packageThree},
		})
	})
	t.Run("it should ignore empty or invalid lines", func(t *testing.T) {
		deps := ParseGhcPkgDepMap("notvalid\n\n")
		assert.Len(t, deps, 0)
	})
}

