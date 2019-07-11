package gradle_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/gradle"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

var DepRuntimeClasspathID = pkg.ID{Type: pkg.Maven, Name: "Implementation", Revision: "2"}
var DepCustomID = pkg.ID{Type: pkg.Maven, Name: "Custom", Revision: "4"}
var ImportsRuntimeClasspath = []pkg.Import{pkg.Import{Resolved: DepRuntimeClasspathID}}
var ImportsCustom = []pkg.Import{pkg.Import{Resolved: DepCustomID}}
var DependenciesRuntimeClasspath = map[pkg.ID]pkg.Package{DepRuntimeClasspathID: pkg.Package{ID: DepRuntimeClasspathID}}
var DependenciesCustom = map[pkg.ID]pkg.Package{DepCustomID: pkg.Package{ID: DepCustomID}}

var testMap = map[string]graph.Deps{
	"runtimeClasspath": graph.Deps{Direct: ImportsRuntimeClasspath, Transitive: DependenciesRuntimeClasspath},
	"custom":           graph.Deps{Direct: ImportsCustom, Transitive: DependenciesCustom},
}

func TestGradleDependencies(t *testing.T) {
	mock := MockInput(testMap)
	a := gradle.Analyzer{Input: mock}
	graph, err := a.Analyze()
	assert.NoError(t, err)
	assert.Equal(t, 1, len(graph.Transitive))
	assert.Equal(t, graph.Transitive[DepRuntimeClasspathID].ID, DepRuntimeClasspathID)
	assert.Empty(t, graph.Transitive[DepCustomID].ID)
}

func TestGradleDependenciesAllConfigurations(t *testing.T) {
	mock := MockGradle{ConfigMap: testMap}
	a := gradle.Analyzer{
		Input: mock,
		Options: gradle.Options{
			AllConfigurations: true,
		},
	}
	graph, err := a.Analyze()
	assert.NoError(t, err)
	assert.Equal(t, 2, len(graph.Transitive))
	assert.Equal(t, graph.Transitive[DepRuntimeClasspathID].ID, DepRuntimeClasspathID)
	assert.Equal(t, graph.Transitive[DepCustomID].ID, DepCustomID)
}

func TestGradleDependenciesCustomConfigurations(t *testing.T) {
	mock := MockInput(testMap)
	a := gradle.Analyzer{
		Input: mock,
		Options: gradle.Options{
			Configuration: "compile,custom",
		},
	}
	graph, err := a.Analyze()
	assert.NoError(t, err)
	assert.Equal(t, 1, len(graph.Transitive))
	assert.Empty(t, graph.Transitive[DepRuntimeClasspathID].ID)
	assert.Equal(t, graph.Transitive[DepCustomID].ID, DepCustomID)
}

func TestGradleDiscovery(t *testing.T) {
	modules, err := gradle.DiscoverWithCommand("testdata", make(map[string]interface{}), mockCommand("testdata/gradle-tasks-all"))
	assert.NoError(t, err)
	assert.Equal(t, 2, len(modules))
	assert.True(t, moduleExists("testdata/grpc-netty", modules))
	assert.True(t, moduleExists("testdata/grpc-xds", modules))
}

func mockCommand(mockOutput string) func(string, string, int, ...string) (string, error) {
	return func(string, string, int, ...string) (string, error) {
		output, err := ioutil.ReadFile(mockOutput)
		return string(output), err
	}
}

type MockGradle struct {
	ConfigMap map[string]graph.Deps
}

func (m MockGradle) ProjectDependencies(args ...string) (map[string]graph.Deps, error) {
	return m.ConfigMap, nil
}

func (m MockGradle) DependencyTasks() ([]string, error) {
	return []string{}, nil
}

func MockInput(config map[string]graph.Deps) MockGradle {
	return MockGradle{
		ConfigMap: config,
	}
}

func moduleExists(name string, modules []module.Module) bool {
	for _, module := range modules {
		if module.Name == name {
			return true
		}
	}
	return false
}
