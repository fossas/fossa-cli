package ruby_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/ruby"
	"github.com/fossas/fossa-cli/buildtools/bundler"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestCustomGemfileLockPath(t *testing.T) {
	buildTarget := "testdata"
	m := module.Module{
		Name:        "test",
		Type:        pkg.Ruby,
		BuildTarget: buildTarget,
		Dir:         buildTarget,
	}

	gemModule := m
	gemModule.Options = map[string]interface{}{
		"strategy": "lockfile",
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

func TestFallbackOnMissingBundler(t *testing.T) {
	buildTarget := "testdata"
	useLockfileOptions := map[string]interface{}{
		"strategy": "lockfile",
	}

	useBundlerWithLockfilePathOptions := map[string]interface{}{}

	gemModuleUsingLockfile := module.Module{
		Name:        "test",
		Type:        pkg.Ruby,
		BuildTarget: buildTarget,
		Dir:         buildTarget,
		Options:     useLockfileOptions,
	}

	gemModuleUsingFallbackToLockfile := module.Module{
		Name:        "test",
		Type:        pkg.Ruby,
		BuildTarget: buildTarget,
		Options:     useBundlerWithLockfilePathOptions,
		Dir:         buildTarget,
	}

	lockfileBasedAnalyzer, lockfileBasedAnalyzerErr := ruby.New(gemModuleUsingLockfile)
	assert.NoError(t, lockfileBasedAnalyzerErr)

	fallbackBasedAnalyzer, fallbackBasedAnalyzerErr := ruby.New(gemModuleUsingFallbackToLockfile)
	assert.NoError(t, fallbackBasedAnalyzerErr)

	fallbackBasedAnalyzer.Bundler = bundler.Bundler{
		Cmd: "doesntWork",
	}

	lockfileBasedAnalyze, lockfileBasedAnalyzerErr := lockfileBasedAnalyzer.Analyze()
	assert.NoError(t, lockfileBasedAnalyzerErr)

	fallbackBasedAnalyze, fallbackBasedAnalyzerErr := fallbackBasedAnalyzer.Analyze()
	assert.NoError(t, fallbackBasedAnalyzerErr)

	// ensure that the arrays are actually populated and we aren't comparing equivalent empty results
	assert.NotEmpty(t, fallbackBasedAnalyze.Direct)
	assert.NotEmpty(t, lockfileBasedAnalyze.Direct)
	assert.Equal(t, fallbackBasedAnalyze, lockfileBasedAnalyze)
}
