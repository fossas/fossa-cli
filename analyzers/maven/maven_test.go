package maven_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/maven"
)

func TestDiscover(t *testing.T) {
	modules, err := maven.Discover("testdata", nil)
	assert.NoError(t, err)

	assert.Equal(t, 4, len(modules))

	p1 := modules[0]
	assert.Equal(t, "Project 1 Sample", p1.Name)
	assert.Equal(t, "pom.xml", p1.BuildTarget)
	assert.Equal(t, "testdata", p1.Dir)

	p2 := modules[1]
	assert.Equal(t, "Project Sample", p2.Name)
	assert.Equal(t, "pom.xml", p2.BuildTarget)
	assert.Equal(t, "testdata/nested", p2.Dir)

	p3 := modules[2]
	assert.Equal(t, "Other Project", p3.Name)
	assert.Equal(t, "pom-other.xml", p3.BuildTarget)
	assert.Equal(t, "testdata/nested", p3.Dir)

	p4 := modules[3]
	assert.Equal(t, "Deep Nested Project", p4.Name)
	assert.Equal(t, "deep-nested/pom.xml", p4.BuildTarget)
	assert.Equal(t, "testdata/nested", p4.Dir)
}
