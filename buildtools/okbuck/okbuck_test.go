package okbuck_test

import (
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/okbuck"
	"github.com/fossas/fossa-cli/pkg"
)

func TestOkBuck(t *testing.T) {
	temp := MockOkBuck("testdata/buckw-targets", "")
	testGraph, err := temp.Deps("")
	assert.NoError(t, err)
	assertImport(t, testGraph.Direct, "dep:one")
	assertImport(t, testGraph.Direct, "dep:two")

	dep1, err := findPackage(testGraph.Transitive, "dep:two")
	assert.NoError(t, err)
	assert.Empty(t, dep1.Imports)
}

func TestOkBuckClassPath(t *testing.T) {
	temp := MockOkBuck("testdata/buckw-targets", "testdata/buckw-classpath")
	testGraph, err := temp.Deps("classpath")
	assert.NoError(t, err)
	assertImport(t, testGraph.Direct, "dep:one")
	assertImport(t, testGraph.Direct, "dep:three")

	dep1, err := findPackage(testGraph.Transitive, "dep:three")
	assert.NoError(t, err)
	assert.Empty(t, dep1.Imports)
}

func MockOkBuck(file string, classpath string) okbuck.OkBuck {
	return okbuck.Setup{
		Target: "test",
		Cmd: func(command string, temp ...string) (string, error) {
			switch command {
			case "targets":
				return testFile(file)
			case "audit":
				return testFile(classpath)
			default:
				return testFile(file)
			}
		},
	}
}

func testFile(file string) (string, error) {
	contents, err := ioutil.ReadFile(file)
	if err != nil {
		return "", err
	}
	return string(contents), nil
}

func findPackage(packages map[pkg.ID]pkg.Package, name string) (pkg.Package, error) {
	for id := range packages {
		if id.Name == name {
			return packages[id], nil
		}
	}
	return pkg.Package{}, fmt.Errorf("Package %s not found", name)
}

func assertImport(t *testing.T, imports pkg.Imports, name string) {
	for _, importedProj := range imports {
		if importedProj.Resolved.Name == name {
			return
		}
	}
	assert.Fail(t, "missing "+name)
}
