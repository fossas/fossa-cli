package composer_test

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/analyzers/php"
	"github.com/fossas/fossa-cli/buildtools/composer"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// A mockRunner implements the composer.Runner interface.
type mockRunner struct {
	showOutput, installOutput string
}

func (mr mockRunner) RunShow(dir string, args ...string) (stdout string, stderr string, err error) {
	return mr.showOutput, "", nil
}

func (mr mockRunner) RunInstall(dir string, args ...string) (stdout string, stderr string, err error) {
	return mr.installOutput, "", nil
}

func TestNoDependenciesNamedPHP(t *testing.T) {
	// TODO: this really shouldn't require the build tool, but we don't currently
	// have a non-build-tool method or mock for Composer. We should implement this
	// by adding resolution from walking `composer.json` dependency manifests.
	if testing.Short() {
		t.Skip("Composer requires build tool")
	}

	// Run analysis.
	m := module.Module{
		Name:        "fixture",
		Type:        pkg.Composer,
		BuildTarget: filepath.Join("testdata", "composer.json"),
		Dir:         "testdata",
	}

	a, err := analyzers.New(m)
	assert.NoError(t, err)
	assert.IsType(t, &php.Analyzer{}, a)

	deps, err := a.Analyze()
	assert.NoError(t, err)

	// Ensure no PHP dependencies.
	for _, dep := range deps.Direct {
		assert.NotEqual(t, "php", dep.Resolved.Name)
	}

	for id, dep := range deps.Transitive {
		assert.NotEqual(t, "php", id.Name)
		for _, i := range dep.Imports {
			assert.NotEqual(t, "php", i.Resolved.Name)
		}
	}
}

func TestComposer_Show(t *testing.T) {
	// A special case with just "[]" output by `composer show --format=json` when no dependencies are found.
	/*
		{
		  "name": "something/something",
		  "description": "Something something.",
		  "type": "something-something",
		  "keywords": [],
		  "license": "GPL-2.0-or-later",
		  "require": {
			"php": ">=7.2.0"
		  }
		}
	*/

	compMock := composer.Composer{
		Runner: mockRunner{
			showOutput: "[]",
		},
	}

	// The point of this is Show should be able to unmarshal the JSON.
	show, err := compMock.Show("")
	assert.NoError(t, err)

	assert.Empty(t, show.Installed)
}
