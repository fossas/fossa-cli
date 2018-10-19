package maven_test

import (
	"io/ioutil"
	"os/exec"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/buildtools/maven"
)

func TestParseDependencyTreeDOS(t *testing.T) {
	// Check that the file is still dos formatted.
	dosFile := "testdata/dos.out"
	out, _ := exec.Command("file", dosFile).Output()
	assert.Equal(t, "testdata/dos.out: ASCII text, with CRLF line terminators\n", string(out))

	dat, err := ioutil.ReadFile(dosFile)
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}

func TestParseDependencyTreeUnix(t *testing.T) {
	// Check that the file is still unix formatted.
	osxFile := "testdata/osx.out"
	out, _ := exec.Command("file", osxFile).Output()
	assert.Equal(t, "testdata/osx.out: ASCII text\n", string(out))

	dat, err := ioutil.ReadFile(osxFile)
	assert.NoError(t, err)
	direct, transitive, err := maven.ParseDependencyTree(string(dat))
	assert.NoError(t, err)
	assert.NotEmpty(t, direct)
	assert.NotEmpty(t, transitive)
}
