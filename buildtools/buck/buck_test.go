package buck_test

import (
	"encoding/json"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/buildtools/buck"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDirectDeps(t *testing.T) {
	fossa.MockOrgID = "1"
	testEnv := Mock("help", nil)

	deps, err := testEnv.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 1, len(deps.Direct))
	assertImport(t, deps.Direct, "test-two")
}

func TestTransitiveDeps(t *testing.T) {
	fossa.MockOrgID = "1"
	testEnv := Mock("help", nil)

	deps, err := testEnv.Deps(false)
	assert.NoError(t, err)
	assert.Equal(t, 2, len(deps.Transitive))
	packageTwo := findPackage(deps.Transitive, "test-two")
	assert.NotZero(t, packageTwo)
	assert.Equal(t, 1, len(packageTwo.Imports))
	assertImport(t, packageTwo.Imports, "test-three")
}

// Mock constructs a buck.Cmd using mock build tool output.
func Mock(file string, err error) buck.Cmd {
	return buck.Cmd{
		Target: "//buck/test:one",
		Audit: func(cmd, target string, args ...string) (buck.AuditOutput, error) {
			switch cmd {
			case "input":
				return buckAuditFile("testdata/input.json"), nil
			case "dependencies":
				switch target {
				case "//buck/test:one":
					if len(args) > 0 && args[0] == "--transitive" {
						return buckAuditFile("testdata/dependenciesTransitive.json"), nil
					}
					return buckAuditFile("testdata/dependencies.json"), nil
				case "//buck/test:two":
					return buckAuditFile("testdata/dependenciesDepTwo.json"), nil
				case "//buck/test:three":
					return buckAuditFile("testdata/dependenciesDepThree.json"), nil
				default:
					return buck.AuditOutput{}, nil
				}
			default:
				return buck.AuditOutput{}, nil
			}
		},
	}
}

func buckAuditFile(file string) buck.AuditOutput {
	var output buck.AuditOutput
	fileRead, err := ioutil.ReadFile(file)
	if err != nil {
		return output
	}
	json.Unmarshal([]byte(fileRead), &output.OutputMapping)
	return output
}

func findPackage(packages map[pkg.ID]pkg.Package, name string) pkg.Package {
	for id := range packages {
		if id.Name == name {
			return packages[id]
		}
	}
	return pkg.Package{}
}

func assertImport(t *testing.T, imports pkg.Imports, name string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			return
		}
	}
	assert.Fail(t, "missing "+name)
}
