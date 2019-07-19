package pipenv_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/pipenv"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/pkg"
)

/*
	├── a@1.0.0
	└─┬ b@2.0.0
      ├─┬ c@3.0.0
      │	└── d@4.0.0
      └── e@5.0.0
*/
func TestDirectDeps(t *testing.T) {
	file, err := ioutil.ReadFile("testdata/get-deps/pipenv-graph-json-tree.json")
	assert.Nil(t, err)
	testEnv := Mock(file, nil)

	deps, error := testEnv.Deps()
	assert.Nil(t, error)

	imports := deps.Direct
	assert.NotZero(t, imports)
	assert.Equal(t, 2, len(imports))
	assertImport(t, imports, "a", "1.0.0")
	assertImport(t, imports, "b", "2.0.0")
}

func TestTransitiveDeps(t *testing.T) {
	file, err := ioutil.ReadFile("testdata/get-deps/pipenv-graph-json-tree.json")
	assert.Nil(t, err)
	testEnv := Mock(file, nil)

	deps, err := testEnv.Deps()
	assert.Nil(t, err)

	graph := deps.Transitive
	assert.NotZero(t, graph)

	packageA := findPackage(graph, "a", "1.0.0")
	assert.NotZero(t, packageA)
	assert.Equal(t, 1, len(packageA.Imports))
	assertImport(t, packageA.Imports, "e", "5.0.0")

	packageB := findPackage(graph, "b", "2.0.0")
	assert.NotZero(t, packageB)
	assert.Equal(t, 2, len(packageB.Imports))
	assertImport(t, packageB.Imports, "c", "3.0.0")
	assertImport(t, packageB.Imports, "e", "5.0.0")

	packageC := findPackage(graph, "c", "3.0.0")
	assert.NotZero(t, packageC)
	assert.Equal(t, 1, len(packageC.Imports))
	assertImport(t, packageC.Imports, "d", "4.0.0")

	packageD := findPackage(graph, "d", "4.0.0")
	assert.NotZero(t, packageD)
	assert.Equal(t, 0, len(packageD.Imports))

	packageE := findPackage(graph, "e", "5.0.0")
	assert.NotZero(t, packageE)
	assert.Equal(t, 0, len(packageE.Imports))
}

// Mock constructs a pipenv.Cmd using mock build tool output.
func Mock(file []byte, err error) pipenv.Cmd {
	return pipenv.Cmd{
		Graph: func(string) (string, *errors.Error) {
			return string(file), nil
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
