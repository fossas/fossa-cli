package rpm_test

import (
	"io/ioutil"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/rpm"
	"github.com/fossas/fossa-cli/testing/helpers"
	"github.com/stretchr/testify/assert"
)

func TestRPMSystemPackages(t *testing.T) {
	cmd := mockRPM("testdata/system-packages-license.txt")
	graph, err := cmd.SystemPackages()
	assert.Nil(t, err)
	assert.Len(t, graph.Transitive, 2)
	assert.Len(t, graph.Direct, 2)

	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")
	helpers.AssertPackageImport(t, graph.Direct, "dep-two", "194aa168af610f7abb3b3e9a9902b518")

	// Check that the hash is being calculated from the license directory file.
	depOneFromFile := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")
	assert.NotEmpty(t, depOneFromFile)

	// Check that the hash is NOT being calculated from the output because the file exists.
	depOneFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "7ee5c4a4b69e9b5bac7e6c581af43804")
	assert.Empty(t, depOneFromOutput)

	// Check that the hash is being calculated from the output because the file does NOT exist.
	depTwoFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-two", "194aa168af610f7abb3b3e9a9902b518")
	assert.NotEmpty(t, depTwoFromOutput)

}

func mockRPM(filename string) rpm.Shell {
	return rpm.Shell{
		LicenseDirectory: "testdata/licenses",
		Upload:           false,
		Cmd: func(...string) (string, error) {
			file, err := ioutil.ReadFile(filename)
			return string(file), err
		},
	}
}
