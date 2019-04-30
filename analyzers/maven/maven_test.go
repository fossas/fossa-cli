package maven_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/maven"
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
