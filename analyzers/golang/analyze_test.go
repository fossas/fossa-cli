package golang_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// TestProjectNotInDeps ensures that the main Go project is not also listed as
// a transitive dependency.
func TestProjectNotInDeps(t *testing.T) {
	// TODO: use a different build target? Use a fixture?
	buildTarget := "github.com/fossas/fossa-cli/cmd/fossa"
	analyzer, err := golang.New(module.Module{Name: "test", Type: pkg.Go, BuildTarget: buildTarget})
	assert.NoError(t, err)

	main, err := analyzer.Go.ListOne([]string{}, buildTarget)
	assert.NoError(t, err)

	deps, err := analyzer.Analyze()
	assert.NoError(t, err)

	for _, dep := range deps.Transitive {
		assert.NotEqual(t, main.ImportPath, dep.ID.Name)
	}
}
