package gradle_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/gradle"
	"github.com/fossas/fossa-cli/pkg"
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
			assertDosFile(t, data)
		} else if file == unix {
			assertUnixFile(t, data)
		}

		g := MockGradle(t, file)
		graph, err := gradle.Dependencies("argument-not-needed-for-mock", g)
		assert.NoError(t, err)

		direct := graph["test"].Direct
		assert.Equal(t, 3, len(direct))
		assertImport(t, direct, "core", "")
		assertImport(t, direct, "dep:one", "1.0")
		assertImport(t, direct, "dep:two", "2.0")

		transitive := graph["test"].Transitive
		assert.Equal(t, 6, len(transitive))

		packageProject := findPackage(transitive, "core", "")
		assert.NotEmpty(t, packageProject)
		assert.Equal(t, 0, len(packageProject.Imports))

		packageOne := findPackage(transitive, "dep:one", "1.0")
		assert.NotEmpty(t, packageOne)
		assert.Equal(t, 1, len(packageOne.Imports))
		assertImport(t, packageOne.Imports, "dep:three", "3.0")

		packageTwo := findPackage(transitive, "dep:two", "2.0")
		assert.NotEmpty(t, packageTwo)
		assert.Equal(t, 2, len(packageTwo.Imports))
		assertImport(t, packageTwo.Imports, "dep:three", "3.0")
		assertImport(t, packageTwo.Imports, "dep:five", "5.0")

		packageThree := findPackage(transitive, "dep:three", "3.0")
		assert.NotEmpty(t, packageThree)
		assert.Equal(t, 1, len(packageThree.Imports))
		assertImport(t, packageThree.Imports, "dep:four", "4.0")

		packageFour := findPackage(transitive, "dep:four", "4.0")
		assert.NotEmpty(t, packageFour)
		assert.Equal(t, 0, len(packageFour.Imports))

		packageFive := findPackage(transitive, "dep:five", "5.0")
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
}

func TestShellCommand_DependencyTasks(t *testing.T) {
	// We should be able to identify the projects by running "gradle tasks" even if a project has the
	// word "dependencies" in it.
	cmd := MockGradle(t, "testdata/tasks-output.txt")
	projects, err := cmd.DependencyTasks()
	assert.NoError(t, err)
	assert.Equal(t, []string{"dependencies-proj"}, projects)
}

func MockGradle(t *testing.T, file string) gradle.ShellCommand {
	fileContents, err := ioutil.ReadFile(file)
	assert.NoError(t, err)
	return gradle.ShellCommand{
		Cmd: func(tmp string, args ...string) (string, error) {
			return string(fileContents), nil
		},
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

func assertUnixFile(t *testing.T, file []byte) {
	fixture := string(file)
	for i := range fixture {
		if i == 0 {
			continue
		}
		if fixture[i] == '\n' {
			assert.NotEqual(t, uint8('\r'), fixture[i-1])
		}
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
