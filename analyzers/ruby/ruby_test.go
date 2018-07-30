package ruby_test

import (
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestCustomGemfileLockPath(t *testing.T) {
	buildTarget := "github.com/fossas/fossa-cli/cmd/fossa"

	analyzer, err := ruby.New(map[string]interface{}{
		"strategy":          "lockfile",
		"gemfile-lock-path": filepath.Join("testdata", "Gemfile.lock"),
	})
	assert.NoError(t, err)

	customLockfileAnalyzer, err := ruby.New(map[string]interface{}{
		"strategy":          "lockfile",
		"gemfile-lock-path": filepath.Join("testdata", "CustomGemfile.lock"),
	})
	assert.NoError(t, err)

	m := module.Module{
		Name:        "test",
		Type:        pkg.Ruby,
		BuildTarget: buildTarget,
	}

	analyzed, err := analyzer.Analyze(m)
	assert.NoError(t, err)

	analyzedCustom, err := customLockfileAnalyzer.Analyze(m)
	assert.NoError(t, err)

	assert.NotEqual(t, analyzed.Deps, analyzedCustom.Deps)
}
