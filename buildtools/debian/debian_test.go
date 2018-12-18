package debian_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/debian"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDependencies(t *testing.T) {
	cmd := Mock()
	graph, err := cmd.Dependencies("depOne")
	assert.NoError(t, err)
	assert.Equal(t, 1, len(graph.Direct))

	assert.Equal(t, 2, len(graph.Transitive))
	depOnePackage := findPackage(graph.Transitive, "depOne", "df453766e8a8e0a25efd4f1c984c397c")
	assert.Equal(t, 1, len(depOnePackage.Imports))
	assertImport(t, depOnePackage.Imports, "depTwo", "e8e6206fc3b3f4bc6efa6f009fbf2f37")
	depTwoPackage := findPackage(graph.Transitive, "depTwo", "e8e6206fc3b3f4bc6efa6f009fbf2f37")
	assert.Equal(t, 0, len(depTwoPackage.Imports))
}

// Mock constructs debian.Cmd using mock build tool output.
func Mock() debian.Cmd {
	return debian.Cmd{
		Directory: "testdata/dependencies",
		Upload:    false,
		DebCmd: func(arguments ...string) (string, error) {
			switch arguments[0] {
			case "depOne":
				file, err := ioutil.ReadFile("testdata/dependencies/depOne")
				return string(file), err
			case "depTwo":
				file, err := ioutil.ReadFile("testdata/dependencies/depTwo")
				return string(file), err
			case "--recurse":
				file, err := ioutil.ReadFile("testdata/dependencies/depOneTransitive")
				return string(file), err
			default:
				return "", nil
			}
		},
	}
}

func findPackage(packages map[pkg.ID]pkg.Package, name, revision string) pkg.Package {
	for id := range packages {
		if id.Name == name && id.Revision == revision {
			return packages[id]
		}
	}
	return pkg.Package{}
}

func assertImport(t *testing.T, imports pkg.Imports, name, revision string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			if importedProj.Resolved.Revision == revision {
				return
			}
			assert.Fail(t, "found "+name+"@"+importedProj.Resolved.Revision+" instead of "+revision)
		}
	}
	assert.Fail(t, "missing "+name+"@"+revision)
}
