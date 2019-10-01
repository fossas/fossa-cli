package gradle_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/testing/helpers"
)

/*
	├─┬ dep:one:1.0.0
	| └─┬ dep:three:3.0.0
  	|   └── dep:four:4.0.0
	└─┬ dep:two:2.0.0
	  ├─┬ dep:three:3.0.0
	  │ └── dep:four:4.0.0
	  └── dep:five:5.0.0
*/

func TestAllDependencies(t *testing.T) {
	dos := "testdata/complete-dos"
	unix := "testdata/complete-unix"
	for _, file := range []string{dos, unix} {
		data, err := ioutil.ReadFile(file)
		assert.NoError(t, err)
		if file == dos {
			helpers.AssertDosFile(t, data)
		} else if file == unix {
			helpers.AssertUnixFile(t, data)
		}

		g := MockGradle(t, file)
		graph, err := gradle.Dependencies("argument-not-needed-for-mock", g)
		assert.NoError(t, err)

		direct := graph["test"].Direct
		assert.Equal(t, 3, len(direct))
		helpers.AssertPackageImport(t, direct, "core", "")
		helpers.AssertPackageImport(t, direct, "dep:one", "1.0")
		helpers.AssertPackageImport(t, direct, "dep:two", "2.0")

		transitive := graph["test"].Transitive
		assert.Equal(t, 6, len(transitive))

		packageProject := helpers.PackageInTransitiveGraph(transitive, "core", "")
		assert.NotEmpty(t, packageProject)
		assert.Equal(t, 0, len(packageProject.Imports))

		packageOne := helpers.PackageInTransitiveGraph(transitive, "dep:one", "1.0")
		assert.NotEmpty(t, packageOne)
		assert.Equal(t, 1, len(packageOne.Imports))
		helpers.AssertPackageImport(t, packageOne.Imports, "dep:three", "3.0")

		packageTwo := helpers.PackageInTransitiveGraph(transitive, "dep:two", "2.0")
		assert.NotEmpty(t, packageTwo)
		assert.Equal(t, 2, len(packageTwo.Imports))
		helpers.AssertPackageImport(t, packageTwo.Imports, "dep:three", "3.0")
		helpers.AssertPackageImport(t, packageTwo.Imports, "dep:five", "5.0")

		packageThree := helpers.PackageInTransitiveGraph(transitive, "dep:three", "3.0")
		assert.NotEmpty(t, packageThree)
		assert.Equal(t, 1, len(packageThree.Imports))
		helpers.AssertPackageImport(t, packageThree.Imports, "dep:four", "4.0")

		packageFour := helpers.PackageInTransitiveGraph(transitive, "dep:four", "4.0")
		assert.NotEmpty(t, packageFour)
		assert.Equal(t, 0, len(packageFour.Imports))

		packageFive := helpers.PackageInTransitiveGraph(transitive, "dep:five", "5.0")
		assert.NotEmpty(t, packageFive)
		assert.Equal(t, 0, len(packageFive.Imports))
	}
}

func TestParseDependencies(t *testing.T) {
	data, err := ioutil.ReadFile("testdata/complex.txt")
	assert.NoError(t, err)
	imports, deps, err := gradle.ParseDependencies(string(data))
	assert.NoError(t, err)

	// A simple dependency.
	expectImport1 := gradle.Dependency{
		Name:             "io.springfox:springfox-swagger2",
		RequestedVersion: "2.9.2",
		ResolvedVersion:  "2.9.2",
	}
	assert.Contains(t, imports, expectImport1)

	// Without a requested version.
	expectImport2 := gradle.Dependency{
		Name:             "org.springframework.boot:spring-boot-starter",
		RequestedVersion: "",
		ResolvedVersion:  "2.1.0.RELEASE",
	}
	assert.Contains(t, imports, expectImport2)

	// With a requested version and resolved version.
	expectDep1 := gradle.Dependency{
		Name:             "com.fasterxml:classmate",
		RequestedVersion: "1.3.4",
		ResolvedVersion:  "1.4.0",
	}
	assert.Contains(t, deps, expectDep1)

	// With a requested version and resolved version.
	expectDep2 := gradle.Dependency{
		Name:             "org.slf4j:slf4j-api",
		RequestedVersion: "1.6.4",
		ResolvedVersion:  "1.7.25",
	}
	assert.Contains(t, deps, expectDep2)

	// A project.
	expectProject1 := gradle.Dependency{
		Name:             "typical-project-name",
		RequestedVersion: "",
		ResolvedVersion:  "",
		IsProject:        true,
	}
	assert.Contains(t, deps, expectProject1)

	// A project.
	expectProject2 := gradle.Dependency{
		Name:             "no-colons-in-project-name",
		RequestedVersion: "",
		ResolvedVersion:  "",
		IsProject:        true,
	}
	assert.Contains(t, deps, expectProject2)
}

func TestDeps(t *testing.T) {
	firstDeps, err := ioutil.ReadFile("testdata/project-one")
	assert.NoError(t, err)
	secondDeps, err := ioutil.ReadFile("testdata/project-two")
	assert.NoError(t, err)
	tasks, err := ioutil.ReadFile("testdata/tasks-output.txt")
	assert.NoError(t, err)
	mockCommand := gradle.ShellCommand{
		Cmd: func(_ string, _ string, _ int, args ...string) (string, error) {
			switch args[0] {
			case "tasks":
				return string(tasks), nil
			case "project-one:dependencies":
				return string(firstDeps), nil
			case "project-two:dependencies":
				return string(secondDeps), nil
			default:
				return "", nil
			}
		},
	}

	deps, depError := gradle.DepsWithCommand(mockCommand, "", "")
	assert.Nil(t, depError)

	assert.Len(t, deps.Direct, 6)
	helpers.AssertPackageImport(t, deps.Direct, "project-one", "")
	helpers.AssertPackageImport(t, deps.Direct, "project-two", "")
	helpers.AssertPackageImport(t, deps.Direct, "dep:one", "1.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "dep:two", "2.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "dep:four", "4.0.0")
	helpers.AssertPackageImport(t, deps.Direct, "dep:five", "5.0.0")

	assert.Len(t, deps.Transitive, 7)

	projectOne := helpers.PackageInTransitiveGraph(deps.Transitive, "project-one", "")
	assert.Len(t, projectOne.Usage, 1)
	assert.Contains(t, projectOne.Usage, "three")
	assert.Len(t, projectOne.Imports, 4)
	helpers.AssertPackageImport(t, projectOne.Imports, "dep:one", "1.0.0")
	helpers.AssertPackageImport(t, projectOne.Imports, "dep:two", "2.0.0")
	helpers.AssertPackageImport(t, projectOne.Imports, "dep:four", "4.0.0")
	helpers.AssertPackageImport(t, projectOne.Imports, "project-two", "")

	projectTwo := helpers.PackageInTransitiveGraph(deps.Transitive, "project-two", "")
	assert.Len(t, projectTwo.Usage, 1)
	assert.Contains(t, projectTwo.Usage, "three")
	assert.Len(t, projectTwo.Imports, 3)
	helpers.AssertPackageImport(t, projectTwo.Imports, "dep:one", "1.0.0")
	helpers.AssertPackageImport(t, projectTwo.Imports, "dep:five", "5.0.0")
	helpers.AssertPackageImport(t, projectTwo.Imports, "project-one", "")

	depOne := helpers.PackageInTransitiveGraph(deps.Transitive, "dep:one", "1.0.0")
	assert.Len(t, depOne.Usage, 4)
	assert.Contains(t, depOne.Usage, "one")
	assert.Contains(t, depOne.Usage, "three")
	assert.Contains(t, depOne.Usage, "four")
	assert.Contains(t, depOne.Usage, "five")
	assert.Len(t, depOne.Imports, 0)

	depTwo := helpers.PackageInTransitiveGraph(deps.Transitive, "dep:two", "2.0.0")
	assert.Len(t, depTwo.Usage, 2)
	assert.Contains(t, depTwo.Usage, "two")
	assert.Contains(t, depTwo.Usage, "three")
	assert.Len(t, depTwo.Imports, 1)
	helpers.AssertPackageImport(t, depTwo.Imports, "dep:three", "3.0.0")

	depThree := helpers.PackageInTransitiveGraph(deps.Transitive, "dep:three", "3.0.0")
	assert.Len(t, depThree.Usage, 2)
	assert.Contains(t, depThree.Usage, "two")
	assert.Contains(t, depThree.Usage, "three")
	assert.Len(t, depThree.Imports, 0)

	depFour := helpers.PackageInTransitiveGraph(deps.Transitive, "dep:four", "4.0.0")
	assert.Len(t, depFour.Usage, 1)
	assert.Contains(t, depFour.Usage, "three")
	assert.Len(t, depFour.Imports, 0)

	depFive := helpers.PackageInTransitiveGraph(deps.Transitive, "dep:five", "5.0.0")
	assert.Len(t, depFive.Usage, 2)
	assert.Contains(t, depFive.Usage, "three")
	assert.Contains(t, depFive.Usage, "four")
	assert.Len(t, depFive.Imports, 1)
	helpers.AssertPackageImport(t, depFive.Imports, "dep:one", "1.0.0")
}

func TestShellCommand_DependencyTasks(t *testing.T) {
	// We should be able to identify the projects by running "gradle tasks" even if a project has the
	// word "dependencies" in it.
	cmd := MockGradle(t, "testdata/tasks-output.txt")
	projects, err := cmd.DependencyTasks()
	assert.NoError(t, err)
	assert.Equal(t, []string{"project-one", "project-two"}, projects)
}

func MockGradle(t *testing.T, file string) gradle.ShellCommand {
	fileContents, err := ioutil.ReadFile(file)
	assert.NoError(t, err)
	return gradle.ShellCommand{
		Cmd: func(string, string, int, ...string) (string, error) {
			return string(fileContents), nil
		},
	}
}
