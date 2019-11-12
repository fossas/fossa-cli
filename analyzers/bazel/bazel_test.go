package bazel_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/bazel"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/testing/helpers"
)

func TestDiscovery(t *testing.T) {
	modules, err := bazel.Discover(".", make(map[string]interface{}))
	assert.NoError(t, err)
	assert.Len(t, modules, 2)
	helpers.AssertModuleExists(t, modules, pkg.Bazel, "BUILD", "testdata", "BUILD")
	helpers.AssertModuleExists(t, modules, pkg.Bazel, "BUILD.bazel", "testdata", "BUILD.bazel")
}
