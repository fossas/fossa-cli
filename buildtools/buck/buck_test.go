package buck_test

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/buildtools/buck"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDirectDeps(t *testing.T) {
	fossa.MockOrgID = "1"
	testEnv := Mock()

	direct, err := testEnv.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 1, len(direct.Direct))
	assertImport(t, direct.Direct, "test-two")

	transitive, err := testEnv.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 2, len(transitive.Transitive))

	packageTwo, err := findPackage(transitive.Transitive, "test-two")
	assert.NoError(t, err)
	assert.Equal(t, 1, len(packageTwo.Imports))
	assertImport(t, packageTwo.Imports, "test-three")

	packageThree, err := findPackage(transitive.Transitive, "test-three")
	assert.NoError(t, err)
	assert.Equal(t, 0, len(packageThree.Imports))
}

func TestDepsAllSubprojects(t *testing.T) {
	fossa.MockOrgID = "1"
	testBuck := mockSubProjects()

	direct, err := testBuck.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 3, len(direct.Direct))
	assertImport(t, direct.Direct, "test-one")
	assertImport(t, direct.Direct, "test-two")
	assertImport(t, direct.Direct, "test-three")

	transitive, err := testBuck.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 3, len(transitive.Transitive))

	packageOne, err := findPackage(transitive.Transitive, "test-one")
	assert.NoError(t, err)
	assert.Equal(t, 1, len(packageOne.Imports))
	assertImport(t, packageOne.Imports, "test-two")

	packageTwo, err := findPackage(transitive.Transitive, "test-two")
	assert.NoError(t, err)
	assert.Equal(t, 1, len(packageTwo.Imports))
	assertImport(t, packageTwo.Imports, "test-three")

	packageThree, err := findPackage(transitive.Transitive, "test-three")
	assert.NoError(t, err)
	assert.Equal(t, 0, len(packageThree.Imports))
}

// Mock constructs a buck.Cmd using mock build tool output.
func Mock() buck.Setup {
	return buck.Setup{
		Target: "//buck/test:one",
		Cmd: func(cmd string, args ...string) (string, error) {
			switch cmd {
			case "root":
				return os.Getwd()
			case "audit":
				switch args[0] {
				case "input":
					return testFile("testdata/input.json")
				case "dependencies":
					switch args[2] {
					case "//buck/test:one":
						if len(args) > 3 && args[3] == "--transitive" {
							return testFile("testdata/dependenciesTransitive.json")
						}
						return testFile("testdata/dependencies.json")
					case "//buck/test:two":
						return testFile("testdata/dependenciesDepTwo.json")
					case "//buck/test:three":
						return testFile("testdata/dependenciesDepThree.json")
					default:
						return "", nil
					}
				default:
					return "", nil
				}
			default:
				return "", nil
			}
		},
	}
}

func mockSubProjects() buck.Setup {
	return buck.Setup{
		Target: "//buck/allprojects/...",
		Cmd: func(cmd string, args ...string) (string, error) {
			switch cmd {
			case "root":
				return os.Getwd()
			case "audit":
				switch args[0] {
				case "input":
					return testFile("testdata/inputAllSubprojects.json")
				case "dependencies":
					switch args[2] {
					case "//buck/test:one":
						return testFile("testdata/dependencies.json")
					case "//buck/test:two":
						return testFile("testdata/dependenciesDepTwo.json")
					case "//buck/test:three":
						return testFile("testdata/dependenciesDepThree.json")
					default:
						return "", nil
					}
				default:
					return "", nil
				}
			case "targets":
				return testFile("testdata/targets.txt")
			default:
				return "", nil
			}
		},
	}
}

func testFile(file string) (string, error) {
	contents, err := ioutil.ReadFile(file)
	if err != nil {
		return "", err
	}
	return string(contents), nil
}

func findPackage(packages map[pkg.ID]pkg.Package, name string) (pkg.Package, error) {
	for id := range packages {
		if id.Name == name {
			return packages[id], nil
		}
	}
	return pkg.Package{}, fmt.Errorf("Package %s not found", name)
}

func assertImport(t *testing.T, imports pkg.Imports, name string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			return
		}
	}
	assert.Fail(t, "missing "+name)
}
