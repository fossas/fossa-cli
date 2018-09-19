// +build integration
 package nodejs
 import (
	"os"
	"testing"
 	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/test/testdata_project_initializers/node"
	"github.com/fossas/fossa-cli/test/testtools"
	"github.com/stretchr/testify/assert"
)
 var nodeInitializer = node.New()
 func TestMain(m *testing.M) {
	teardownTests, err := testtools.SetupTests(nodeInitializer)
	if err != nil {
		if err.Error() != "repository already exists" {
			panic(err)
		}
	}
 	exitCode := m.Run()
	defer os.Exit(exitCode)
	defer teardownTests()
}
 // While not testing the core functionality, this ensures that the tests have been setup correctly as needed for a prereq to run the analyzer steps
// This test itself does not incur any overhead.
func TestTestSetup(t *testing.T) {
	assertProjectFixtureExists(t, "puppeteer")
	// faker has no deps
	// assertProjectFixtureExists(t, "fakerjs")
	assertProjectFixtureExists(t, "fastify")
	assertProjectFixtureExists(t, "nest")
	assertProjectFixtureExists(t, "ohm")
	assertProjectFixtureExists(t, "express")
	assertProjectFixtureExists(t, "standard")
	assertProjectFixtureExists(t, "sodium-encryption")
	assertProjectFixtureExists(t, "request")
}
 func assertProjectFixtureExists(t *testing.T, name string) {
	exists, err := files.ExistsFolder(nodeInitializer.FixtureDirectory(), name)
	assert.NoError(t, err)
	assert.True(t, exists, name+" was not properly cloned")
 	exists, err = files.ExistsFolder(nodeInitializer.FixtureDirectory(), name, "node_modules")
	assert.NoError(t, err)
	assert.True(t, exists, name+" did not have its node modules installed")
}