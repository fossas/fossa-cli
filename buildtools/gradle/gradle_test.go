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
	dos := "testdata/dos"
	unix := "testdata/unix"
	for _, file := range []string{unix, dos} {
		data, err := ioutil.ReadFile(file)
		assert.NoError(t, err)
		if file == dos {
			assertDosFile(t, data)
		}

		direct, transitive, err := gradle.ParseDependencies(string(data))
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

func assertDosFile(t *testing.T, file []byte) {
	fixture := string(file)
	for i := range fixture {
		if i == 0 {
			continue
		}
		if fixture[i] == '\n' {
			assert.Equal(t, uint8('\r'), fixture[i-1])
		}
	}
}

func TestAllDependencies(t *testing.T) {
	for _, file := range []string{"testdata/complete-dos"} {
		g := MockGradle(t, file)
		graph, err := g.Dependencies("")
		assert.NoError(t, err)
		assert.Equal(t, 6, len(graph))
	}
}

func MockGradle(t *testing.T, file string) gradle.Gradle {
	fileContents, err := ioutil.ReadFile(file)
	assert.NoError(t, err)
	return gradle.Gradle{
		Setup: gradle.Setup{
			Cmd: func(tmp string, args ...string) (string, error) {
				return string(fileContents), nil
			},
		},
	}
}
