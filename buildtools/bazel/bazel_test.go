package bazel_test

import (
	"fmt"
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
				fmt.Println(err)
			}
			return string(file), nil
		},
	}

	graph, err := shell.Command("//test/...", false)
	assert.NoError(t, err)

	assert.Len(t, graph.Direct, 5)
	helpers.AssertPackageImport(t, graph.Direct, "github.com/organization/project/package-one", "")
	helpers.AssertPackageImport(t, graph.Direct, "package-two", "2.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "package-three", "3.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "@package/four", "4.0.0")
	helpers.AssertPackageImport(t, graph.Direct, "package-five", "5.0.0")

	assert.Len(t, graph.Transitive, 5)
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
}
