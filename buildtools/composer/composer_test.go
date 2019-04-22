package composer_test

import (
	"fmt"
	"reflect"
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
            "description": "Polyfill for Mbstring"
        },
        {
            "name": "twig/twig",
            "version": "v2.5.0",
            "description": "Twig, the flexible, fast, and secure template language for PHP"
        }
    ]
}`

	type pkg = composer.Package

	p1 := pkg{"twig/twig", "v2.5.0", "Twig, the flexible, fast, and secure template language for PHP"}
	p2 := pkg{"symfony/polyfill-ctype", "v1.9.0", "Symfony polyfill for ctype functions"}
	p3 := pkg{"symfony/polyfill-mbstring", "v1.9.0", "Polyfill for Mbstring"}

	runner := mockRunner{
		showTreeOutput: tree,
		showOutput:     show,
	}

	imports, deps, err := composer.Dependencies("", runner)
	assert.NoError(t, err)

	// Check for all expected dependencies, and ensure that "php" is not one of them.
	expectedImports := []pkg{p1}

	assert.Equal(t, len(expectedImports), len(imports), fmt.Sprintf("Got: %v", imports))
	assert.Equal(t, expectedImports, imports)

	expectedDeps := map[string][]pkg{
		"twig/twig":                 {p2, p3},
		"symfony/polyfill-ctype":    {},
		"symfony/polyfill-mbstring": {},
	}

	for pkgName, pkgDeps := range expectedDeps {
		var found *pkg
		for k := range deps {
			if k.Name == pkgName {
				found = &k
				break
			}
		}
		assert.NotNil(t, found)

		// The pkgDeps slice and the corresponding slice in deps may have all the same data but with the
		// elements not in the same order because they're taken from iteration over a map.
		for _, dep := range pkgDeps {
			// Find a Package with the same data.
			sameFound := false
			for _, gotDep := range deps[*found] {
				if reflect.DeepEqual(dep, gotDep) {
					sameFound = true
					break
				}
			}
			assert.True(t, sameFound)
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
