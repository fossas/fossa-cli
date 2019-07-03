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

var testPoms = filepath.Join("testdata", "poms")

func TestModules(t *testing.T) {
	// Here we mostly just test the discovery of POM files, and analyzers/maven tests that we get the correct
	// list of MvnModules.

	simplePom := filepath.Join(testPoms, "pom.xml")
	checked := make(map[string]bool)
	mods, err := maven.Modules(simplePom, testPoms, checked)
	assert.NoError(t, err)
	assert.Len(t, mods, 1)
	exists, err := files.Exists(mods[0].Dir, mods[0].Target)
	assert.NoError(t, err)
	assert.True(t, exists)

	// Now that pom.xml in testPoms has been checked, make sure we don't check it again.
	modsAgain, err := maven.Modules(simplePom, testPoms, checked)
	assert.NoError(t, err)
	assert.Nil(t, modsAgain)

	// Make sure we follow references to other modules (module as path to a file) listed in the POM file.
	pomWithModules := filepath.Join(testPoms, "nested", "pom.xml")
	mods2, err := maven.Modules(pomWithModules, testPoms, make(map[string]bool))
	assert.NoError(t, err)
	assert.Len(t, mods2, 3)
	for _, mod := range mods2 {
		exists, err := files.Exists(mod.Dir, mod.Target)
		assert.NoError(t, err)
		assert.True(t, exists)
	}

	// Make sure we follow references to other modules (module as path to a directory) listed in the POM file.
	pomWithModules2 := filepath.Join(testPoms, "pom-minimal.xml")
	mods3, err := maven.Modules(pomWithModules2, testPoms, make(map[string]bool))
	assert.NoError(t, err)
	assert.Len(t, mods3, 4)
	for _, mod := range mods3 {
		exists, err := files.Exists(mod.Dir, mod.Target)
		assert.NoError(t, err)
		assert.True(t, exists)
	}

	// Test the fallback to artifact ID if name is not given in the manifest.
	assert.Contains(t, mods3, maven.MvnModule{Name: "minimal", Target: "pom-minimal.xml", Dir: testPoms})
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
