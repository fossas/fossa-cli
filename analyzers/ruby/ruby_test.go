package ruby_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestCustomGemfileLockPath(t *testing.T) {
	buildTarget := "github.com/fossas/fossa-cli/cmd/fossa"
	m := module.Module{
		Name:        "test",
		Type:        pkg.Ruby,
		BuildTarget: buildTarget,
	}

	gemModule := m
	gemModule.Options = map[string]interface{}{
		"strategy":          "lockfile",
		"gemfile-lock-path": filepath.Join("testdata", "Gemfile.lock"),
	}
	analyzer, err := ruby.New(gemModule)
	assert.NoError(t, err)

	customModule := m
	customModule.Options = map[string]interface{}{
		"strategy":          "lockfile",
		"gemfile-lock-path": filepath.Join("testdata", "CustomGemfile.lock"),
	}
	customLockfileAnalyzer, err := ruby.New(customModule)
	assert.NoError(t, err)

	analyzed, err := analyzer.Analyze()
	assert.NoError(t, err)

	analyzedCustom, err := customLockfileAnalyzer.Analyze()
	assert.NoError(t, err)

	assert.NotEqual(t, analyzed, analyzedCustom)
}
