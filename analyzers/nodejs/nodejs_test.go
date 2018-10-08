package nodejs_test

import (
	"log"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// TestNoDependencies checks that there is no error even when `package.json` is
// missing a `dependencies` key or has an empty object as the value for
// `dependencies`.
func TestNoDependencies(t *testing.T) {
	m := module.Module{
		Dir:  filepath.Join("testdata", "empty"),
		Type: pkg.NodeJS,
	}

	a, err := analyzers.New(m)
	assert.NoError(t, err)

	a.(*nodejs.Analyzer).NPM = MockNPM{
		JSONFilename: filepath.Join("testdata", "empty", "npm-ls-json.json"),
	}

	deps, err := a.Analyze()
	assert.NoError(t, err)
	assert.Empty(t, deps.Direct)
	assert.Empty(t, deps.Transitive)
}

// TestDuplicateDependencies checks that analysis correctly handles duplicate
// dependencies, even in the case where duplicates may not have the same set of
// imports listed.
//
// For example, try running `npm ls --json` in the `testdata/duplicates` folder.
// Notice that `babel-runtime` is included as a dependency twice: once by
// `babel-polyfill` and once by `jira-client`. However, the _dependencies_ of
// `babel-runtime` are only listed _once,_ when it's imported by
// `babel-polyfill`. This means that we must ensure that we get transitive
// dependencies from the original dependency entry, not the deduplicated entry.
//
// See #257 for details.
func TestDuplicateDependencies(t *testing.T) {
	m := module.Module{
		BuildTarget: filepath.Join("testdata", "duplicates", "package.json"),
		Dir:         filepath.Join("testdata", "duplicates"),
		Type:        pkg.NodeJS,
	}

	a, err := analyzers.New(m)
	assert.NoError(t, err)

	a.(*nodejs.Analyzer).NPM = MockNPM{
		JSONFilename: filepath.Join("testdata", "duplicates", "npm-ls-json.json"),
	}

	// We run this multiple times because this bug may flake; map traversal order
	// is random in Go.
	var failed pkg.Deps
	for i := 0; i < 10; i++ {
		deps, err := a.Analyze()
		assert.NoError(t, err)
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     "regenerator-runtime",
			Revision: "0.11.1",
			Location: "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.11.1.tgz",
		}
		ok := assert.Contains(t, deps.Transitive, id)
		if !ok {
			failed = deps.Transitive
		}
	}

	if t.Failed() {
		log.Printf("%#v", failed)
	}
}

var chaiDirectDep = pkg.Import{
	Target: "chai",
	Resolved: pkg.ID{
		Name:     "chai",
		Revision: "4.1.2",
		Type:     pkg.NodeJS,
	},
}

var npmChaiFixtures = []string{
	filepath.Join("testdata", "chai", "installed"),
	filepath.Join("testdata", "chai", "installed-lockfile"),
	filepath.Join("testdata", "chai", "dev-deps"),
}

func TestAnalyzeWithNpmLs(t *testing.T) {
	t.Parallel()
	for _, fixturePath := range npmChaiFixtures {
		t.Run(fixturePath, func(t *testing.T) {
			t.Parallel()
			testAnalyzeWithNpmLs(t, fixturePath)

		})
	}
}

func testAnalyzeWithNpmLs(t *testing.T, buildTarget string) {
	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule)
	assert.NoError(t, err)

	analyzer.NPM = MockNPM{
		JSONFilename: filepath.Join(buildTarget, "npm-ls-json.json"),
	}

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	assert.Len(t, analysisResults.Direct, 1)
	assert.Len(t, analysisResults.Transitive, 7)
}

func TestUsingNodeModuleFallback(t *testing.T) {
	t.Parallel()
	for _, fixturePath := range npmChaiFixtures {
		t.Run(fixturePath, func(t *testing.T) {
			t.Parallel()
			testUsingNodeModuleFallback(t, fixturePath)

		})
	}
}

func testUsingNodeModuleFallback(t *testing.T, buildTarget string) {
	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule)
	assert.NoError(t, err)

	analyzer.NPM = MockNPMFailure{}

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	chaiProject := analysisResults.Transitive[chaiDirectDep.Resolved]
	assert.NotNil(t, chaiProject)
	AssertImport(t, chaiProject.Imports, "assertion-error", "1.1.0")
	AssertImport(t, chaiProject.Imports, "check-error", "1.0.2")
	AssertImport(t, chaiProject.Imports, "get-func-name", "2.0.0")
	AssertImport(t, chaiProject.Imports, "pathval", "1.1.0")
	AssertImport(t, chaiProject.Imports, "deep-eql", "3.0.1")
	AssertImport(t, chaiProject.Imports, "type-detect", "4.0.8")
}

func AssertImport(t *testing.T, imports pkg.Imports, name, revision string) {
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

func TestUsingYarnLockfileFallback(t *testing.T) {
	buildTarget := filepath.Join("testdata", "chai", "installed-yarn-lockfile")

	nodeModule := module.Module{
		Name:        "test",
		Type:        pkg.NodeJS,
		BuildTarget: buildTarget,
		Options:     map[string]interface{}{},
	}

	analyzer, err := nodejs.New(nodeModule)
	assert.NoError(t, err)

	analyzer.NPM = MockNPMFailure{}

	analysisResults, err := analyzer.Analyze()
	assert.NoError(t, err)

	chaiProject := analysisResults.Transitive[chaiDirectDep.Resolved]
	AssertImport(t, chaiProject.Imports, "assertion-error", "1.1.0")
	AssertImport(t, chaiProject.Imports, "check-error", "1.0.2")
	AssertImport(t, chaiProject.Imports, "get-func-name", "2.0.0")
	AssertImport(t, chaiProject.Imports, "pathval", "1.1.0")
	AssertImport(t, chaiProject.Imports, "deep-eql", "3.0.1")
	AssertImport(t, chaiProject.Imports, "type-detect", "4.0.8")
}
