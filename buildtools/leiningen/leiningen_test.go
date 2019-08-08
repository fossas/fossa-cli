package leiningen_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/leiningen"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestShellDependencies(t *testing.T) {
	shell := leiningen.Shell{
		Cmd: func(...string) (string, *errors.Error) {
			file, err := ioutil.ReadFile("testdata/leindepstree.txt")
			if err != nil {
				return "", errors.UnknownError(err, "")
			}
			return string(file), nil
		},
	}

	deps, err := shell.DependencyGraph("target")
	assert.Nil(t, err)
	assert.Equal(t, 3, len(deps.Direct))
	helpers.AssertPackageImport(t, deps.Direct, "project:project", "")
	helpers.AssertPackageImport(t, deps.Direct, "one:one", "1.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "organization:two", "2.0.0")

	assert.Equal(t, 6, len(deps.Transitive))
	packageProject := helpers.PackageInTransitiveGraph(deps.Transitive, "project:project", "")
	assert.NotEmpty(t, packageProject)
	assert.Equal(t, 0, len(packageProject.Imports))

	packageOne := helpers.PackageInTransitiveGraph(deps.Transitive, "one:one", "1.0.0")
	assert.NotEmpty(t, packageOne)
	assert.Equal(t, 1, len(packageOne.Imports))
	helpers.AssertPackageImport(t, packageOne.Imports, "three:three", "3.0.0")

	packageTwo := helpers.PackageInTransitiveGraph(deps.Transitive, "organization:two", "2.0.0")
	assert.NotEmpty(t, packageTwo)
	assert.Equal(t, 2, len(packageTwo.Imports))
	helpers.AssertPackageImport(t, packageTwo.Imports, "three:three", "3.0.0")
	helpers.AssertPackageImport(t, packageTwo.Imports, "five:five", "5.0.0")

	packageThree := helpers.PackageInTransitiveGraph(deps.Transitive, "three:three", "3.0.0")
	assert.NotEmpty(t, packageThree)
	assert.Equal(t, 1, len(packageThree.Imports))
	helpers.AssertPackageImport(t, packageThree.Imports, "organization:four", "4.0.0")

	packageFour := helpers.PackageInTransitiveGraph(deps.Transitive, "organization:four", "4.0.0")
	assert.NotEmpty(t, packageFour)
	assert.Equal(t, 0, len(packageFour.Imports))

	packageFive := helpers.PackageInTransitiveGraph(deps.Transitive, "five:five", "5.0.0")
	assert.NotEmpty(t, packageFive)
	assert.Equal(t, 0, len(packageFive.Imports))
}

func TestFileDependencies(t *testing.T) {
	deps, err := leiningen.ProjectFile("testdata", "test.clj")
	assert.Nil(t, err)
	assert.Equal(t, 3, len(deps.Direct))
	helpers.AssertPackageImport(t, deps.Direct, "one:one", "1.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "organization:two", "2.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "three:three", "3.0.0")

	packageOne := helpers.PackageInTransitiveGraph(deps.Transitive, "one:one", "1.0.0")
	assert.NotEmpty(t, packageOne)
	assert.Empty(t, packageOne.Imports)

	packageTwo := helpers.PackageInTransitiveGraph(deps.Transitive, "organization:two", "2.0.0")
	assert.NotEmpty(t, packageTwo)
	assert.Empty(t, packageTwo.Imports)

	packageThree := helpers.PackageInTransitiveGraph(deps.Transitive, "three:three", "3.0.0")
	assert.NotEmpty(t, packageThree)
	assert.Empty(t, packageThree.Imports)
}
