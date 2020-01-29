package bazel_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/bazel"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestBazelCommand(t *testing.T) {
	shell := bazel.Shell{
		Cmd: func(...string) (string, *errors.Error) {
			file, err := files.Read("testdata/cmd/bazel.xml")
			if err != nil {
				t.Fatal("could not read the test file")
			}
			return string(file), nil
		},
	}

	// Test rule with double slash prefix.
	graph, err := shell.TargetDependencies("//test/...", false)
	assert.NoError(t, err)
	assert.Len(t, graph.Direct, 6)

	graph, err = shell.TargetDependencies("test/...", false)
	assert.NoError(t, err)

	assert.Len(t, graph.Direct, 6)
	helpers.AssertPackageImport(t, graph.Direct, "github.com/organization/project/package-one", "")
	helpers.AssertPackageImport(t, graph.Direct, "package-two", "2.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "package-three", "3.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "@package/four", "4.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "package-five", "5.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "@package-six", "")

	assert.Len(t, graph.Transitive, 6)
	packageOne := helpers.PackageInTransitiveGraph(graph.Transitive, "github.com/organization/project/package-one", "")
	assert.NotEmpty(t, packageOne)
	assert.Equal(t, pkg.Go, packageOne.ID.Type)
	assert.Len(t, packageOne.Imports, 0)

	packageTwo := helpers.PackageInTransitiveGraph(graph.Transitive, "package-two", "2.0.0")
	assert.NotEmpty(t, packageTwo)
	assert.Equal(t, pkg.Rust, packageTwo.ID.Type)
	assert.Len(t, packageTwo.Imports, 0)

	packageThree := helpers.PackageInTransitiveGraph(graph.Transitive, "package-three", "3.0.0")
	assert.NotEmpty(t, packageThree)
	assert.Equal(t, pkg.NodeJS, packageThree.ID.Type)
	assert.Len(t, packageThree.Imports, 0)

	packageFour := helpers.PackageInTransitiveGraph(graph.Transitive, "@package/four", "4.0.0")
	assert.NotEmpty(t, packageFour)
	assert.Equal(t, pkg.NodeJS, packageFour.ID.Type)
	assert.Len(t, packageFour.Imports, 0)

	packageFive := helpers.PackageInTransitiveGraph(graph.Transitive, "package-five", "5.0.0")
	assert.NotEmpty(t, packageFive)
	assert.Equal(t, pkg.Python, packageFive.ID.Type)
	assert.Len(t, packageFive.Imports, 0)

	packageSix := helpers.PackageInTransitiveGraph(graph.Transitive, "@package-six", "")
	assert.NotEmpty(t, packageSix)
	assert.Equal(t, pkg.NodeJS, packageSix.ID.Type)
	assert.Len(t, packageSix.Imports, 0)
}
