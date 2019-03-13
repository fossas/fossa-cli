package gradle_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/gradle"
)

/*
	├── dep:one:1.0.0
	└─┬ dep:two:2.0.0
	  ├─┬ dep:three:3.0.0
	  │ └── dep:four:4.0.0
	  └── dep:five:5.0.0
*/

var depProject = gradle.Dependency{Name: "core", IsProject: true}
var depOne = gradle.Dependency{Name: "dep:one", Resolved: "1.0", Target: "1.0", IsProject: false}
var depTwo = gradle.Dependency{Name: "dep:two", Resolved: "2.0", Target: "2.0", IsProject: false}
var depThree = gradle.Dependency{Name: "dep:three", Resolved: "3.0", Target: "3.0", IsProject: false}
var depFour = gradle.Dependency{Name: "dep:four", Resolved: "4.0", Target: "4.0", IsProject: false}
var depFive = gradle.Dependency{Name: "dep:five", Resolved: "5.0", Target: "5.0", IsProject: false}

func TestParseDependencyTree(t *testing.T) {
	files := []string{"testdata/unix", "testdata/dos"}
	for _, file := range files {
		dat, err := ioutil.ReadFile(file)
		assert.NoError(t, err)
		direct, transitive, err := gradle.ParseDependencies(string(dat))
		assert.NoError(t, err)

		assert.Equal(t, 3, len(direct))
		assert.Contains(t, direct, depProject)
		assert.Contains(t, direct, depOne)
		assert.Contains(t, direct, depTwo)

		assert.Equal(t, 6, len(transitive))
		assert.Contains(t, transitive, depProject)
		assert.Contains(t, transitive, depOne)
		assert.Contains(t, transitive, depTwo)
		assert.Contains(t, transitive[depTwo], depThree)
		assert.Contains(t, transitive[depTwo], depFive)
		assert.Contains(t, transitive, depThree)
		assert.Contains(t, transitive[depThree], depFour)
		assert.Contains(t, transitive, depFour)
		assert.Contains(t, transitive, depFive)
	}
}
