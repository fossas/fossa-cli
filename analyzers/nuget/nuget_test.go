package nuget_test

import (
	"fmt"
	"testing"

	"github.com/fossas/fossa-cli/analyzers/nuget"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/stretchr/testify/assert"
)

func TestIdealPath(t *testing.T) {
	testAnalyzer := newAnalyzer("testdata/fallbacks/good-obj-directory/test.csproj", "testdata/fallbacks/good-obj-directory")
	graph, err := testAnalyzer.Analyze()
	assert.NoError(t, err)
	assert.NotEmpty(t, graph)
	assertImport(t, graph.Direct, "test-package-assets")
}
func TestFallbackToPackageReference(t *testing.T) {
	testAnalyzer := newAnalyzer("testdata/fallbacks/no-obj-directory/test.csproj", "testdata/fallbacks/no-obj-directory")
	graph, err := testAnalyzer.Analyze()
	assert.NoError(t, err)
	assert.NotEmpty(t, graph)
	assertImport(t, graph.Direct, "not-package-assets")
}
func TestFallbackToNuspec(t *testing.T) {
	testAnalyzer := newAnalyzer("testdata/fallbacks/bad-package-ref/temp.nuspec", "testdata/fallbacks/bad-package-ref")
	graph, err := testAnalyzer.Analyze()
	assert.NoError(t, err)
	assert.NotEmpty(t, graph)
	assertImport(t, graph.Direct, "nuspec-fallback")
}

func assertImport(t *testing.T, imports []pkg.Import, name string) {
	for _, dep := range imports {
		if dep.Resolved.Name == name {
			return
		}
	}
	assert.Fail(t, fmt.Sprintf("Imports list: %+v\nDoes not contain\nDependency: %s", imports, name))
}

func newAnalyzer(target, dir string) nuget.Analyzer {
	return nuget.Analyzer{
		Module: module.Module{
			BuildTarget: target,
			Dir:         dir,
		},
	}
}
