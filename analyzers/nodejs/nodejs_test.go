package nodejs_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestNoDependencies(t *testing.T) {
	t.Skip("unimplemented")
}

func TestAnalyzeWithNpmLs(t *testing.T) {
	buildTarget := "fixtures/with_node_modules/"

	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule)
	assert.NoError(t, err)

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	assert.NotEmpty(t, analysisResults.Direct)
	assert.NotEmpty(t, analysisResults.Transitive)
}

func TestUsingNodeModuleFallback(t *testing.T) {
	t.Skip("not yet implemented")
	buildTarget := "fixtures/with_node_modules"

	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule)
	assert.NoError(t, err)

	analyzer.NPMCmd = "badNpmCommand"

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	assert.NotEmpty(t, analysisResults.Direct)
}
