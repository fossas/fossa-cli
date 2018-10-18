package maven_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
)

func TestParseDependencyTreeDOS(t *testing.T) {
	dat, err := ioutil.ReadFile("testdata/dos.out")
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}

func TestParseDependencyTreeUnix(t *testing.T) {
	dat, err := ioutil.ReadFile("testdata/osx.out")
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}
