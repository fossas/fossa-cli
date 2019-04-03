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

var DepAPIID = pkg.ID{Type: pkg.Maven, Name: "API", Revision: "1"}
var DepImplmentationID = pkg.ID{Type: pkg.Maven, Name: "Implementation", Revision: "2"}
var DepCompileID = pkg.ID{Type: pkg.Maven, Name: "Compile", Revision: "3"}
var DepCustomID = pkg.ID{Type: pkg.Maven, Name: "Custom", Revision: "4"}
var ImportsAPI = []pkg.Import{pkg.Import{Resolved: DepAPIID}}
var ImportsImplementation = []pkg.Import{pkg.Import{Resolved: DepImplmentationID}}
var ImportsCompile = []pkg.Import{pkg.Import{Resolved: DepCompileID}}
var ImportsCustom = []pkg.Import{pkg.Import{Resolved: DepCustomID}}
var DependenciesAPI = map[pkg.ID]pkg.Package{DepAPIID: pkg.Package{ID: DepAPIID}}
var DependenciesImplementation = map[pkg.ID]pkg.Package{DepImplmentationID: pkg.Package{ID: DepImplmentationID}}
var DependenciesCompile = map[pkg.ID]pkg.Package{DepCompileID: pkg.Package{ID: DepCompileID}}
var DependenciesCustom = map[pkg.ID]pkg.Package{DepCustomID: pkg.Package{ID: DepCustomID}}

var testMap = map[string]graph.Deps{
	"api":            graph.Deps{Direct: ImportsAPI, Transitive: DependenciesAPI},
	"implementation": graph.Deps{Direct: ImportsImplementation, Transitive: DependenciesImplementation},
	"compile":        graph.Deps{Direct: ImportsCompile, Transitive: DependenciesCompile},
	"custom":         graph.Deps{Direct: ImportsCustom, Transitive: DependenciesCustom},
}

func TestGradleDependencies(t *testing.T) {
	mock := MockInput(testMap)
	a := gradle.Analyzer{Input: mock}
	graph, err := a.Analyze()
	assert.NoError(t, err)
	assert.Equal(t, 3, len(graph.Transitive))
	assert.Equal(t, graph.Transitive[DepAPIID].ID, DepAPIID)
	assert.Equal(t, graph.Transitive[DepImplmentationID].ID, DepImplmentationID)
	assert.Equal(t, graph.Transitive[DepCompileID].ID, DepCompileID)
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
	assert.Equal(t, 4, len(graph.Transitive))
	assert.Equal(t, graph.Transitive[DepAPIID].ID, DepAPIID)
	assert.Equal(t, graph.Transitive[DepImplmentationID].ID, DepImplmentationID)
	assert.Equal(t, graph.Transitive[DepCompileID].ID, DepCompileID)
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
	assert.Equal(t, 2, len(graph.Transitive))
	assert.Empty(t, graph.Transitive[DepAPIID].ID)
	assert.Empty(t, graph.Transitive[DepImplmentationID].ID)
	assert.Equal(t, graph.Transitive[DepCompileID].ID, DepCompileID)
	assert.Equal(t, graph.Transitive[DepCustomID].ID, DepCustomID)
}

func TestGradleDiscovery(t *testing.T) {
	modules, err := gradle.DiscoverWithCommand("testdata", make(map[string]interface{}), mockCommand("testdata/gradle-tasks-all"))
	assert.NoError(t, err)
	assert.Equal(t, 2, len(modules))
	assert.True(t, moduleExists("testdata/grpc-netty", modules))
	assert.True(t, moduleExists("testdata/grpc-xds", modules))
}

func mockCommand(mockOutput string) func(string, ...string) (string, error) {
	return func(string, ...string) (string, error) {
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
