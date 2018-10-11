package pipenv_test

import (
	"errors"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/pipenv"
	"github.com/fossas/fossa-cli/pkg"
)

type MockExec struct {
	File  string
	Error error
}

func (m MockExec) DataAccess(s string) (string, error) {
	return m.File, m.Error
}

func newMockPipenv(file []byte, err error) pipenv.SystemPipenv {
	return pipenv.SystemPipenv{
		PipenvExec: MockExec{
			File:  string(file),
			Error: err,
		},
	}
}

/*
	├── a@1.0.0
	└─┬ b@2.0.0
      ├─┬ c@3.0.0
      │	└── d@4.0.0
      └── e@5.0.0
*/
func TestDirectDeps(t *testing.T) {
	file, err := ioutil.ReadFile("testdata/get-deps/pipenv-graph-json-tree.json")
	assert.NoError(t, err)
	testEnv := newMockPipenv(file, nil)

	deps, error := testEnv.Deps()
	assert.NoError(t, error)

	imports := deps.Direct
	assert.NotZero(t, imports)
	assert.Equal(t, 2, len(imports))
	assertImport(t, imports, "a", "1.0.0")
	assertImport(t, imports, "b", "2.0.0")

}

func TestTransitiveDeps(t *testing.T) {
	file, err := ioutil.ReadFile("testdata/get-deps/pipenv-graph-json-tree.json")
	assert.NoError(t, err)
	testEnv := newMockPipenv(file, nil)

	deps, error := testEnv.Deps()
	assert.NoError(t, error)

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

func TestNoFile(t *testing.T) {
	testEnv := newMockPipenv([]byte{}, errors.New("test error"))

	deps, err := testEnv.Deps()
	assert.Zero(t, deps)
	assert.EqualError(t, err, "test error")
}

func TestBadFile(t *testing.T) {
	file, err := ioutil.ReadFile("bad.json")
	testEnv := newMockPipenv(file, nil)

	deps, err := testEnv.Deps()
	assert.Zero(t, deps)
	assert.EqualError(t, err, "could not unmarshall JSON into dependency list: unexpected end of JSON input")
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
