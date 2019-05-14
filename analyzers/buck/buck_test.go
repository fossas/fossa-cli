package buck_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/buck"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func TestDiscoveryAliasConfig(t *testing.T) {
	dir := "testdata/buckconfig/Alias"
	modules, err := buck.DiscoverWithCommand(dir, make(map[string]interface{}), mockBuck)
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 2)
	assert.Contains(t, modules, testModule("one", "//test:one", dir))
	assert.Contains(t, modules, testModule("two", "//test:two", dir))
}

func TestDiscoveryEmptyAliasConfig(t *testing.T) {
	dir := "testdata/buckconfig/EmptyAlias"
	modules, err := buck.DiscoverWithCommand(dir, make(map[string]interface{}), mockBuck)
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 2)
	assert.Contains(t, modules, testModule("//test:one", "//test:one", dir))
	assert.Contains(t, modules, testModule("//test:two", "//test:two", dir))
}

func TestDiscoveryBlankConfig(t *testing.T) {
	dir := "testdata/buckconfig/Blank"
	modules, err := buck.DiscoverWithCommand(dir, make(map[string]interface{}), mockBuck)
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 2)
	assert.Contains(t, modules, testModule("//test:one", "//test:one", dir))
	assert.Contains(t, modules, testModule("//test:two", "//test:two", dir))
}

func TestDiscoveryBUCKFile(t *testing.T) {
	dir := "testdata/BUCK"
	modules, err := buck.DiscoverWithCommand(dir, make(map[string]interface{}), mockBuck)
	assert.NoError(t, err)
	assert.Equal(t, len(modules), 2)
	assert.Contains(t, modules, testModule("//test:one", "//test:one", dir))
	assert.Contains(t, modules, testModule("//test:two", "//test:two", dir))
}

func testModule(name, target, dir string) module.Module {
	return module.Module{
		Name:        name,
		Type:        pkg.Buck,
		BuildTarget: target,
		Dir:         dir,
	}
}

func mockBuck(cmd string, args ...string) (string, error) {
	switch cmd {
	case "root":
		return "", nil
	case "targets":
		return "//test:one\n//test:two", nil
	default:
		return "", errors.New("Cannot identify the test command")
	}
}
