package composer_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/composer"
)

// The mockRunner type implements composer.Composer.
type mockRunner struct {
	showTreeOutput, showOutput, installOutput string
}

func (mr mockRunner) Show(dir string, args ...string) (stdout string, stderr string, err error) {
	for _, arg := range args {
		if arg == "--tree" {
			return mr.showTreeOutput, "", nil
		}
	}
	return mr.showOutput, "", nil
}

func (mr mockRunner) Install(dir string, args ...string) (stdout string, stderr string, err error) {
	return mr.installOutput, "", nil
}

func TestDependencies(t *testing.T) {
	const tree = "twig/twig v2.5.0 Twig, the flexible, fast, and secure template language for PHP\n" +
		"|--php ^7.0\n" +
		"|--symfony/polyfill-ctype ^1.8\n" +
		"|  `--php >=5.3.3\n" +
		"`--symfony/polyfill-mbstring ~1.0\n" +
		"`--php >=5.3.3\n"

	const show = `{
    "installed": [
        {
            "name": "symfony/polyfill-ctype",
            "version": "v1.9.0",
            "description": "Symfony polyfill for ctype functions"
        },
        {
            "name": "symfony/polyfill-mbstring",
            "version": "v1.9.0",
            "description": "Symfony polyfill for the Mbstring extension"
        },
        {
            "name": "twig/twig",
            "version": "v2.5.0",
            "description": "Twig, the flexible, fast, and secure template language for PHP"
        }
    ]
}`

	runner := mockRunner{
		showTreeOutput: tree,
		showOutput:     show,
	}

	imports, deps, err := composer.Dependencies("", runner)
	assert.NoError(t, err)

	assert.Equal(t, 1, len(imports), fmt.Sprintf("Got: %v", imports))
	assert.Equal(t, 3, len(deps), fmt.Sprintf("Got: %v", deps))

	// Ensure there are no PHP dependencies.
	for _, dep := range imports {
		assert.NotEqual(t, "php", dep.Name)
	}

	for id, dep := range deps {
		assert.NotEqual(t, "php", id.Name)
		for _, deepDep := range dep {
			assert.NotEqual(t, "php", deepDep.Name)
		}
	}
}

func TestShow_empty(t *testing.T) {
	// A special case with just "[]" output by `composer show --format=json` when no dependencies are found.
	// The point of this test is that Show should be able to handle the JSON.
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

	runner := mockRunner{}

	runner.showOutput = "[]"
	show, err := composer.Show("", runner)
	assert.NoError(t, err)
	assert.Empty(t, show.Installed)

	runner.showOutput = "[]\n"
	show, err = composer.Show("", runner)
	assert.NoError(t, err)
	assert.Empty(t, show.Installed)
}
