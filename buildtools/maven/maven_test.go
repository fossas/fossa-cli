package maven_test

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/pkg"
)

var testdataDir = filepath.Join("..", "..", "analyzers", "maven", "testdata")

func TestResolveManifestFromBuildTarget(t *testing.T) {
	var m maven.Maven

	// A directory path.
	pom, err := m.ResolveManifestFromBuildTarget(testdataDir)
	if assert.NoError(t, err) {
		assert.Equal(t, "com.domain.name:stuff", pom.GroupID+":"+pom.ArtifactID)
	}

	// A POM file path.
	pom2, err := m.ResolveManifestFromBuildTarget(filepath.Join(testdataDir, "nested", "pom.xml"))
	if assert.NoError(t, err) {
		assert.Equal(t, "com.someone.code.a:gson-extras", pom2.GroupID+":"+pom2.ArtifactID)
	}

	// A project identifier.
	_, err3 := m.ResolveManifestFromBuildTarget("something:else")
	assert.Error(t, err3)
}

func TestModules(t *testing.T) {
	fullPath := filepath.Join(testdataDir, "pom.xml")
	checked := make(map[string]bool)
	mods, err := maven.Modules(fullPath, testdataDir, checked)
	if assert.NoError(t, err) {
		assert.Len(t, mods, 1)
	}

	checked2 := make(map[string]bool)
	dirOnlyPath := filepath.Join(testdataDir, "nested")
	mods2, err := maven.Modules(dirOnlyPath, testdataDir, checked2)
	if assert.NoError(t, err) {
		assert.Len(t, mods2, 2)
	}
}

func TestParseDependencyTreeDOS(t *testing.T) {
	// Check that the file is still DOS formatted.
	data, err := ioutil.ReadFile(filepath.Join("testdata", "dos.out"))
	assert.NoError(t, err)

	fixture := string(data)
	for i := range fixture {
		if i == 0 {
			continue
		}
		if fixture[i] == '\n' {
			assert.Equal(t, uint8('\r'), fixture[i-1])
		}
	}

	direct, transitive, err := maven.ParseDependencyTree(fixture)
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}

func TestParseDependencyTreeUnix(t *testing.T) {
	// Check that the file is still Unix formatted.
	data, err := ioutil.ReadFile(filepath.Join("testdata", "unix.out"))
	assert.NoError(t, err)

	fixture := string(data)
	for i := range fixture {
		if i == 0 {
			continue
		}
		if fixture[i] == '\n' {
			assert.NotEqual(t, '\r', fixture[i-1])
		}
	}

	direct, transitive, err := maven.ParseDependencyTree(fixture)
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}

/*
	├── dep:one:1.0.0
	└─┬ dep:two:2.0.0
	  ├─┬ dep:three:3.0.0
	  │ └── dep:four:4.0.0
	  └── dep:five:5.0.0
*/

var depOne = pkg.ID{Type: pkg.Maven, Name: "dep:one", Revision: "1.0.0"}
var depTwo = pkg.ID{Type: pkg.Maven, Name: "dep:two", Revision: "2.0.0"}
var depThree = pkg.ID{Type: pkg.Maven, Name: "dep:three", Revision: "3.0.0"}
var depFour = pkg.ID{Type: pkg.Maven, Name: "dep:four", Revision: "4.0.0"}
var depFive = pkg.ID{Type: pkg.Maven, Name: "dep:five", Revision: "5.0.0"}

func TestParseDependencyTree(t *testing.T) {
	dat, err := ioutil.ReadFile("testdata/unix.out")
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)

	assert.Equal(t, 2, len(direct))
	assert.Contains(t, direct, pkg.Import{Target: "", Resolved: depOne})
	assert.Contains(t, direct, pkg.Import{Target: "", Resolved: depTwo})

	assert.Equal(t, 5, len(transitive))
	assert.Contains(t, transitive, depOne)
	assert.Contains(t, transitive, depTwo)
	assert.Contains(t, transitive[depTwo].Imports, pkg.Import{Target: "3.0.0", Resolved: depThree})
	assert.Contains(t, transitive[depTwo].Imports, pkg.Import{Target: "5.0.0", Resolved: depFive})
	assert.Contains(t, transitive, depThree)
	assert.Contains(t, transitive[depThree].Imports, pkg.Import{Target: "4.0.0", Resolved: depFour})
	assert.Contains(t, transitive, depFour)
	assert.Contains(t, transitive, depFive)
}
