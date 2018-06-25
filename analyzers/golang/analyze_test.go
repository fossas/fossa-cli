package golang_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestProjectNotInDeps(t *testing.T) {
	// TODO: use a different build target? Use a fixture?
	buildTarget := "github.com/fossas/fossa-cli/cmd/fossa"
	analyzer, err := golang.New(make(map[string]interface{}))
	assert.NoError(t, err)

	main, err := analyzer.Go.ListOne(buildTarget)
	assert.NoError(t, err)

	m := module.Module{
		Name:        "test",
		Type:        pkg.Go,
		BuildTarget: buildTarget,
	}

	analyzed, err := analyzer.Analyze(m)
	assert.NoError(t, err)

	for _, dep := range analyzed.Deps {
		assert.NotEqual(t, main.ImportPath, dep.ID.Name)
	}
}
