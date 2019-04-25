package nuget_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/nuget"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestNugetDiscovery(t *testing.T) {
	packagesConfig := module.Module{
		Name:        "testdata/discovery/packages",
		Type:        pkg.NuGet,
		BuildTarget: "testdata/discovery/packages",
		Dir:         "testdata/discovery/packages",
	}
	nuspec := module.Module{
		Name:        "NUnit",
		Type:        pkg.NuGet,
		BuildTarget: "testdata/discovery/nuspec/temp.nuspec",
		Dir:         "testdata/discovery/nuspec",
	}
	packageReference := module.Module{
		Name:        "TestPackage",
		Type:        pkg.NuGet,
		BuildTarget: "testdata/discovery/package-reference-priority/temp.csproj",
		Dir:         "testdata/discovery/package-reference-priority",
	}
	projectJSON := module.Module{
		Name:        "testdata/discovery/project",
		Type:        pkg.NuGet,
		BuildTarget: "testdata/discovery/project",
		Dir:         "testdata/discovery/project",
	}

	modules, err := nuget.Discover("testdata/discovery", nil)
	assert.NoError(t, err)
	assert.Len(t, modules, 4)
	assert.Contains(t, modules, packagesConfig)
	assert.Contains(t, modules, packageReference)
	assert.Contains(t, modules, projectJSON)
	assert.Contains(t, modules, nuspec)
}
