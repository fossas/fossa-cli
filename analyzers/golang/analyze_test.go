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

	main, err := analyzer.Go.ListOne(buildTarget, nil)
	assert.NoError(t, err)

	deps, err := analyzer.Analyze()
	assert.NoError(t, err)

	for _, dep := range deps.Transitive {
		assert.NotEqual(t, main.ImportPath, dep.ID.Name)
	}
}

var semver = pkg.ID{
	Type:     pkg.Go,
	Name:     "github.com/blang/semver",
	Revision: "2ee87856327ba09384cabd113bc6b5d174e9ec0f",
}

var toml = pkg.ID{
	Type:     pkg.Go,
	Name:     "github.com/BurntSushi/toml",
	Revision: "3012a1dbe2e4bd1391d42b32f0577cb7bbc7f005",
}

func TestProjectWithoutBuildTags(t *testing.T) {
	testModule := module.Module{}
	testModule.BuildTarget = "github.com/fossas/fossa-cli/analyzers/golang/testdata/demotags"

	analyzer, err := golang.New(testModule)
	assert.NoError(t, err)
	deps, err := analyzer.Analyze()
	assert.NoError(t, err)
	assert.Contains(t, deps.Transitive, toml)
	assert.NotContains(t, deps.Transitive, semver)
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
	assert.Contains(t, deps.Transitive, toml)
	assert.Contains(t, deps.Transitive, semver)
}
