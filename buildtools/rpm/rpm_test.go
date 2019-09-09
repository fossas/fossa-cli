package rpm_test

import (
	"io/ioutil"
	"testing"

	"github.com/fossas/fossa-cli/buildtools/rpm"
	"github.com/fossas/fossa-cli/testing/helpers"
	"github.com/stretchr/testify/assert"
)

const licenseDirectory = "testdata/licenses/system-directory"

func TestRPMSystemPackages(t *testing.T) {
	cmd := mockSystemRPM("testdata/licenses/system-packages-licenses")
	graph, err := cmd.SystemPackages()
	assert.Nil(t, err)
	assert.Len(t, graph.Transitive, 2)
	assert.Len(t, graph.Direct, 2)

	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")
	helpers.AssertPackageImport(t, graph.Direct, "dep-two", "d5ecbbf9beaa2aa16372f29d3c94107a")

	// Check that the hash is being calculated from the license directory file.
	depOneFromFile := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")
	assert.NotEmpty(t, depOneFromFile)

	// Check that the hash is NOT being calculated from the output because the file exists.
	depOneFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "7ee5c4a4b69e9b5bac7e6c581af43804")
	assert.Empty(t, depOneFromOutput)

	// Check that the hash is being calculated from the output because the file does NOT exist.
	depTwoFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-two", "d5ecbbf9beaa2aa16372f29d3c94107a")
	assert.NotEmpty(t, depTwoFromOutput)
}

func TestRPMSinglePackage(t *testing.T) {
	cmd := mockSingleRPM()
	graph, err := cmd.SinglePackage("dep-one")
	assert.Nil(t, err)
	assert.Len(t, graph.Transitive, 6)
	assert.Len(t, graph.Direct, 1)

	helpers.AssertPackageImport(t, graph.Direct, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")

	depOneFromFile := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-one", "f8c6ab0503f8cc4f8222da2aecb5e0f0")
	assert.NotEmpty(t, depOneFromFile)
	assert.Len(t, depOneFromFile.Imports, 3)
	helpers.AssertPackageImport(t, depOneFromFile.Imports, "dep-two", "d5ecbbf9beaa2aa16372f29d3c94107a")
	helpers.AssertPackageImport(t, depOneFromFile.Imports, "dep-three", "d5b27bf5f0f90c2584a9b3151af8f54b")
	helpers.AssertPackageImport(t, depOneFromFile.Imports, "dep-rpm", "a3e240aae27ac0bf7799cde1ec6eaa59")

	depTwoFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-two", "d5ecbbf9beaa2aa16372f29d3c94107a")
	assert.NotEmpty(t, depTwoFromOutput)
	assert.Len(t, depTwoFromOutput.Imports, 1)
	helpers.AssertPackageImport(t, depTwoFromOutput.Imports, "dep-four", "86af1dc5f79d4ba51c6c1e4ad9a344ec")

	depThreeFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-three", "d5b27bf5f0f90c2584a9b3151af8f54b")
	assert.NotEmpty(t, depThreeFromOutput)
	assert.Empty(t, depThreeFromOutput.Imports)

	depFourFromFile := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-four", "86af1dc5f79d4ba51c6c1e4ad9a344ec")
	assert.NotEmpty(t, depFourFromFile)
	assert.Len(t, depFourFromFile.Imports, 1)
	helpers.AssertPackageImport(t, depFourFromFile.Imports, "dep-five", "2c0960a66b92a552f1b64ddf47baa715")

	depFiveFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-five", "2c0960a66b92a552f1b64ddf47baa715")
	assert.NotEmpty(t, depFiveFromOutput)
	assert.Empty(t, depFiveFromOutput.Imports)

	depRPMFromOutput := helpers.PackageInTransitiveGraph(graph.Transitive, "dep-rpm", "a3e240aae27ac0bf7799cde1ec6eaa59")
	assert.NotEmpty(t, depRPMFromOutput)
	assert.Empty(t, depRPMFromOutput.Imports)
}

func mockSystemRPM(filename string) rpm.Shell {
	return rpm.Shell{
		LicenseDirectory: licenseDirectory,
		Upload:           false,
		RPM: func(...string) (string, string, error) {
			file, err := ioutil.ReadFile(filename)
			return string(file), "", err
		},
	}
}

func mockSingleRPM() rpm.Shell {
	return rpm.Shell{
		LicenseDirectory: licenseDirectory,
		Upload:           false,
		RPM: func(args ...string) (string, string, error) {
			switch args[0] {
			case "-q":
				if args[1] == "--whatprovides" {
					return "", "", nil
				}
				switch args[len(args)-1] {
				case "dep-one":
					return stringFile("testdata/licenses/dep-one-license")
				case "dep-two":
					return stringFile("testdata/licenses/dep-two-license")
				case "dep-three":
					return stringFile("testdata/licenses/dep-three-license")
				case "dep-four":
					return stringFile("testdata/licenses/dep-four-license")
				case "dep-five":
					return stringFile("testdata/licenses/dep-five-license")
				case "rpm":
					return stringFile("testdata/licenses/dep-rpm-license")
				}
				fallthrough
			case "-qR":
				switch args[1] {
				case "dep-one":
					return stringFile("testdata/dep-one-transitive")
				case "dep-two":
					return stringFile("testdata/dep-two-transitive")
				case "dep-four":
					return stringFile("testdata/dep-four-transitive")
				}
				fallthrough
			default:
				return "", "", nil
			}
		},
		Yum: func(...string) (string, string, error) {
			return "", "", nil
		},
	}
}

func stringFile(fileName string) (string, string, error) {
	file, err := ioutil.ReadFile(fileName)
	return string(file), "", err
}
