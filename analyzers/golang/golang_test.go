package golang_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/analyzers/golang"
	"github.com/fossas/fossa-cli/module"
)

func TestNoNewPanicWithBadOptions(t *testing.T) {
	t.Skip("pending")
}

func TestNewBuildTags(t *testing.T) {
	testModule := module.Module{}
	testModule.Options = make(map[string]interface{})
	testModule.Options["tags"] = []string{"tag1", "tag2"}
	testModule.Options["all-tags"] = true

	analyer, err := golang.New(testModule)
	assert.NoError(t, err)
	assert.Contains(t, analyer.BuildTags, "")
	assert.Contains(t, analyer.BuildTags, "tag1")
	assert.Contains(t, analyer.BuildTags, "tag2")
	assert.Contains(t, analyer.BuildTags, "windows")
	assert.Contains(t, analyer.BuildTags, "386")
}
