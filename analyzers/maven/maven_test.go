package maven_test

import (
	"path/filepath"
	"testing"

	"github.com/fossas/fossa-cli/analyzers/maven"
	"github.com/stretchr/testify/assert"
)

func TestDiscover(t *testing.T) {
	modules, err := maven.Discover("testdata", nil)
	assert.NoError(t, err)

	assert.Equal(t, 3, len(modules))

	p1 := modules[0]
	assert.Equal(t, "Project 1 Sample", p1.Name)
	assert.Equal(t, "testdata/pom.xml", p1.BuildTarget)

	p2 := modules[1]
	assert.Equal(t, "Project Sample", p2.Name)
	assert.Equal(t, "testdata/nested/pom.xml", p2.BuildTarget)

	p3 := modules[2]
	assert.Equal(t, "Other Project", p3.Name)
	assert.Equal(t, "testdata/nested/pom-other.xml", p3.BuildTarget)
}

func TestDepsFromModule(t *testing.T) {
	// target1 names a manifest file.
	target1 := filepath.Join("testdata", "pom.xml")
	// target2 names a module's directory.
	target2 := filepath.Join("testdata", "nested")

	// Here we're testing just that DepsFromModule can determine which file to read and how to read it.
	// The structure of the maven.Manifest file is tested in package buildtools/maven.
	deps1, err := maven.DepsFromModule(target1)
	assert.NoError(t, err)
	assert.Equal(t, 3, len(deps1))

	deps2, err := maven.DepsFromModule(target2)
	assert.NoError(t, err)
	assert.Equal(t, 2, len(deps2))
}
