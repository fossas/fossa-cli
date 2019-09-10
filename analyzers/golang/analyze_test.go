package golang_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/graph"
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

	main, err := analyzer.Go.ListOne(buildTarget, nil, false)
	assert.NoError(t, err)

	deps, err := analyzer.Analyze()
	assert.NoError(t, err)

	for _, dep := range deps.Transitive {
		assert.NotEqual(t, main.ImportPath, dep.ID.Name)
	}
}

func TestPackageNoDeps(t *testing.T) {
	testModule := module.Module{}
	testModule.BuildTarget = "github.com/fossas/fossa-cli/analyzers/golang/testdata/nodeps"

	analyzer, err := golang.New(testModule)
	assert.NoError(t, err)

	testGraph, err := analyzer.Analyze()
	assert.NoError(t, err)
	assert.Equal(t, testGraph, graph.Deps{})
}

var customtag = pkg.ID{
	Type:     pkg.Go,
	Name:     "imports/customtag",
	Revision: "v3.0.0",
}

var combo = pkg.ID{
	Type:     pkg.Go,
	Name:     "imports/combo",
	Revision: "12345",
}

func TestProjectWithoutBuildTags(t *testing.T) {
	testModule := module.Module{}
	testModule.BuildTarget = "github.com/fossas/fossa-cli/analyzers/golang/testdata/demotags"

	analyzer, err := golang.New(testModule)
	assert.NoError(t, err)
	deps, err := analyzer.Analyze()
	assert.NoError(t, err)
	assert.NotContains(t, deps.Transitive, combo)
	assert.NotContains(t, deps.Transitive, customtag)
}

func TestProjectWithBuildTags(t *testing.T) {
	testModule := module.Module{}
	testModule.Options = make(map[string]interface{})
	testModule.Options["tags"] = []string{"customtag"}
	testModule.BuildTarget = "github.com/fossas/fossa-cli/analyzers/golang/testdata/demotags"

	analyzer, err := golang.New(testModule)
	assert.NoError(t, err)
	deps, err := analyzer.Analyze()
	assert.NoError(t, err)
	assert.Contains(t, deps.Transitive, customtag)
	assert.NotContains(t, deps.Transitive, combo)
}

func TestProjectWithMultipleBuildTags(t *testing.T) {
	testModule := module.Module{}
	testModule.Options = make(map[string]interface{})
	testModule.Options["tags"] = []string{"combo1 combo2"}
	testModule.BuildTarget = "github.com/fossas/fossa-cli/analyzers/golang/testdata/demotags"

	analyzer, err := golang.New(testModule)
	assert.NoError(t, err)
	deps, err := analyzer.Analyze()
	assert.NoError(t, err)
	assert.Contains(t, deps.Transitive, combo)
	assert.NotContains(t, deps.Transitive, customtag)
}
