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

func TestPythonDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/python/BUILD", false)
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Python, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestRustDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/rust/BUILD.bazel", false)
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Rust, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestNPMDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/npm/BUILD", false)
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "1.1.1")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "1.1.1")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.NodeJS, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestGoDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/go/BUILD", false)
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Go, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}

func TestCPlusPlusDeps(t *testing.T) {
	graph, err := bazel.Deps("testdata/c++/BUILD", false)
	assert.NoError(t, err)
	helpers.AssertPackageImport(t, graph.Direct, "c++", "c96385546c3e60884a729ce36f2bc33a")

	testPackage := helpers.PackageInTransitiveGraph(graph.Transitive, "c++", "c96385546c3e60884a729ce36f2bc33a")
	assert.NotEmpty(t, testPackage)
	assert.Equal(t, pkg.Raw, testPackage.ID.Type)
	assert.Len(t, testPackage.Imports, 0)
}
