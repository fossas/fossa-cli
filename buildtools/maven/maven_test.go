package maven_test

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
)

func TestResolveManifestFromBuildTarget(t *testing.T) {
	testdataDir := filepath.Join("..", "..", "analyzers", "maven", "testdata")
	var m maven.Maven

	// A directory path.
	pom, err := m.ResolveManifestFromBuildTarget(testdataDir)
	if assert.NoError(t, err) {
		assert.Equal(t, "com.domain.name:stuff", pom.GroupID+":"+pom.ArtifactID)
	}

	// A POM file path.
	path2 := filepath.Join(testdataDir, "nested", "pom.xml")
	pom2, err := m.ResolveManifestFromBuildTarget(path2)
	if assert.NoError(t, err) {
		assert.Equal(t, "com.someone.code.a:gson-extras", pom2.GroupID+":"+pom2.ArtifactID)
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

var depOne = maven.Dependency{Name: "dep:one", Version: "1.0.0", Failed: false}
var depTwo = maven.Dependency{Name: "dep:two", Version: "2.0.0", Failed: false}
var depThree = maven.Dependency{Name: "dep:three", Version: "3.0.0", Failed: false}
var depFour = maven.Dependency{Name: "dep:four", Version: "4.0.0", Failed: false}
var depFive = maven.Dependency{Name: "dep:five", Version: "5.0.0", Failed: false}

func TestParseDependencyTree(t *testing.T) {
	dat, err := ioutil.ReadFile("testdata/unix.out")
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)

	assert.Equal(t, 2, len(direct))
	assert.Contains(t, direct, depOne)
	assert.Contains(t, direct, depTwo)

	assert.Equal(t, 5, len(transitive))
	assert.Contains(t, transitive, depOne)
	assert.Contains(t, transitive, depTwo)
	assert.Contains(t, transitive[depTwo], depThree)
	assert.Contains(t, transitive[depTwo], depFive)
	assert.Contains(t, transitive, depThree)
	assert.Contains(t, transitive[depThree], depFour)
	assert.Contains(t, transitive, depFour)
	assert.Contains(t, transitive, depFive)
}
