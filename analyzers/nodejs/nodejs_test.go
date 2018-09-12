package nodejs_test

import (
	"os"
	"testing"

	"github.com/fossas/fossa-cli/files"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestNoDependencies(t *testing.T) {
	t.Skip("unimplemented")
}

type MockNPM struct{}

func (n MockNPM) List(dir string) (npm.Output, error) {
	var output npm.Output
	dir, _ = os.Getwd()

	files.ReadJSON(&output, dir, "fixtures/npmLsOutput.json")

	return output, nil
}

func (n MockNPM) Clean(dir string) error {
	return nil
}

func (n MockNPM) Install(dir string) error {
	return nil
}

func TestAnalyzeWithNpmLs(t *testing.T) {
	buildTarget := "fixtures/with_node_modules"

	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule, MockNPM{})
	assert.NoError(t, err)

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	assert.Len(t, analysisResults.Direct, 1)
	assert.Len(t, analysisResults.Transitive, 7)
}

func TestUsingNodeModuleFallback(t *testing.T) {
	t.Skip("not yet implemented")
	// buildTarget := "fixtures/with_node_modules/"

	// nodeModule := module.Module{
	// 	Name:        "test",
	// 	Type:        pkg.NodeJS,
	// 	BuildTarget: buildTarget,
	// 	Options:     map[string]interface{}{},
	// }

	// sysTool := nodejs.SystemNpmTool(nodeModule.Options)

	// analyzer, err := nodejs.New(nodeModule)
	// assert.NoError(t, err)

	// analyzer.NPMCmd = "badNpmCommand"

	// analysisResults, err := analyzer.Analyze()
	// assert.NoError(t, err)

	// assert.NotEmpty(t, analysisResults.Direct)
}
