package maven_test

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

var testdataDir = filepath.Join("..", "..", "analyzers", "maven", "testdata")

func TestModules(t *testing.T) {
	// Here we mostly just test the discovery of POM files, and analyzers/maven tests that we get the correct
	// list of MvnModules.

	path1 := filepath.Join(testdataDir, "pom.xml")
	checked := make(map[string]bool)
	mods, err := maven.Modules(path1, testdataDir, checked)
	if assert.NoError(t, err) {
		assert.Len(t, mods, 1)
		for _, mod := range mods {
			exists, err := files.Exists(mod.Dir, mod.Target)
			assert.NoError(t, err)
			assert.True(t, exists)
		}
	}

	// After the pom.xml file in testdataDir has been checked, make sure we don't check it again.
	modsAgain, err := maven.Modules(path1, testdataDir, checked)
	if assert.NoError(t, err) {
		assert.Nil(t, modsAgain)
	}

	// Make sure we follow references to other modules (module as path to a file) listed in the POM file.
	path2 := filepath.Join(testdataDir, "nested", "pom.xml")
	mods2, err := maven.Modules(path2, testdataDir, make(map[string]bool))
	if assert.NoError(t, err) {
		assert.Len(t, mods2, 2)
		for _, mod := range mods2 {
			exists, err := files.Exists(mod.Dir, mod.Target)
			assert.NoError(t, err)
			assert.True(t, exists)
		}
	}

	// Make sure we follow references to other modules (module as path to a directory) listed in the POM file.
	path3 := filepath.Join(testdataDir, "pom-minimal.xml")
	mods3, err := maven.Modules(path3, testdataDir, make(map[string]bool))
	if assert.NoError(t, err) {
		assert.Len(t, mods3, 3)

		// Test fallback to artifact ID if name is not given in the manifest.
		assert.Contains(t, mods3, maven.MvnModule{Name: "minimal", Target: "pom-minimal.xml", Dir: testdataDir})

		for _, mod := range mods3 {
			exists, err := files.Exists(mod.Dir, mod.Target)
			assert.NoError(t, err)
			assert.True(t, exists)
		}
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

	deps, err := maven.ParseDependencyTree(fixture)
	assert.NoError(t, err)
	assert.NotEmpty(t, deps.Direct)
	assert.NotEmpty(t, deps.Transitive)
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

	deps, err := maven.ParseDependencyTree(fixture)
	assert.NoError(t, err)
	assert.NotEmpty(t, deps.Direct)
	assert.NotEmpty(t, deps.Transitive)
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
	deps, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)

	assert.Equal(t, 2, len(deps.Direct))
	assert.Contains(t, deps.Direct, pkg.Import{Target: "", Resolved: depOne})
	assert.Contains(t, deps.Direct, pkg.Import{Target: "", Resolved: depTwo})

	assert.Equal(t, 5, len(deps.Transitive))
	assert.Contains(t, deps.Transitive, depOne)
	assert.Contains(t, deps.Transitive, depTwo)
	assert.Contains(t, deps.Transitive[depTwo].Imports, pkg.Import{Target: "3.0.0", Resolved: depThree})
	assert.Contains(t, deps.Transitive[depTwo].Imports, pkg.Import{Target: "5.0.0", Resolved: depFive})
	assert.Contains(t, deps.Transitive, depThree)
	assert.Contains(t, deps.Transitive[depThree].Imports, pkg.Import{Target: "4.0.0", Resolved: depFour})
	assert.Contains(t, deps.Transitive, depFour)
	assert.Contains(t, deps.Transitive, depFive)
}
