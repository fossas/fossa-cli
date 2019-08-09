package clojure_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/clojure"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestDiscovery(t *testing.T) {
	modules, err := clojure.Discover("testdata", make(map[string]interface{}))
	assert.Nil(t, err)

	helpers.AssertModuleExists(t, modules, pkg.Clojure, "testdata/project.clj", ".", "project.clj")
	helpers.AssertModuleExists(t, modules, pkg.Clojure, "testdata/sub-project/project.clj", "sub-project", "project.clj")
}

var testDepOne = pkg.ID{Type: pkg.Maven, Name: "lein:lein", Revision: "1.0.0"}
var testGraph = graph.Deps{
	Direct:     []pkg.Import{pkg.Import{Target: testDepOne.Name, Resolved: testDepOne}},
	Transitive: map[pkg.ID]pkg.Package{testDepOne: pkg.Package{ID: testDepOne}},
}

func TestSuccessfullLein(t *testing.T) {
	successfullLein := &clojure.Analyzer{
		Module: module.Module{
			BuildTarget: "project.clj",
			Dir:         "testdata",
		},
		Lein: mockLein{graph: testGraph},
	}

	depGraph, err := successfullLein.Analyze()
	assert.Nil(t, err)
	assert.Equal(t, 1, len(depGraph.Direct))
	helpers.AssertPackageImport(t, depGraph.Direct, "lein:lein", "1.0.0")
}

func TestLeinStrategy(t *testing.T) {
	successfullLein := &clojure.Analyzer{
		Module: module.Module{
			BuildTarget: "project.clj",
			Dir:         "testdata",
		},
		Options: clojure.Options{Strategy: "lein"},
		Lein:    mockLein{graph: testGraph},
	}

	depGraph, err := successfullLein.Analyze()
	assert.Nil(t, err)
	assert.Equal(t, 1, len(depGraph.Direct))
	helpers.AssertPackageImport(t, depGraph.Direct, "lein:lein", "1.0.0")
}

func TestFallbackToFile(t *testing.T) {
	successfullFile := &clojure.Analyzer{
		Module: module.Module{
			BuildTarget: "project.clj",
			Dir:         "testdata",
		},
		Lein: mockLein{err: &errors.Error{Cause: errors.New("lein did not work"), Troubleshooting: ""}},
	}

	depGraph, err := successfullFile.Analyze()
	assert.Nil(t, err)
	assert.Equal(t, 1, len(depGraph.Direct))
	helpers.AssertPackageImport(t, depGraph.Direct, "file:file", "2.0.0")
}

func TestFileStrategy(t *testing.T) {
	successfullLein := &clojure.Analyzer{
		Module: module.Module{
			BuildTarget: "project.clj",
			Dir:         "testdata",
		},
		Options: clojure.Options{Strategy: "project.clj"},
		Lein:    mockLein{graph: testGraph},
	}

	depGraph, err := successfullLein.Analyze()
	assert.Nil(t, err)
	assert.Equal(t, 1, len(depGraph.Direct))
	helpers.AssertPackageImport(t, depGraph.Direct, "file:file", "2.0.0")
}

func TestBothFail(t *testing.T) {
	errorLein := &clojure.Analyzer{
		Module: module.Module{
			BuildTarget: "nonexistent",
			Dir:         "testdata",
		},
		Lein: mockLein{err: &errors.Error{Cause: errors.New("lein did not work"), Troubleshooting: ""}},
	}

	depGraph, err := errorLein.Analyze()
	assert.NotEmpty(t, err)
	assert.Equal(t, 0, len(depGraph.Direct))
}

type mockLein struct {
	err   *errors.Error
	graph graph.Deps
}

func (mock mockLein) DependencyGraph(_ string) (graph.Deps, *errors.Error) {
	if mock.err != nil {
		return graph.Deps{}, mock.err
	}
	return mock.graph, nil
}
