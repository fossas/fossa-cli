package upload_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestUploadCmdHasEndpointFlag(t *testing.T) {
	assert.Subset(t, upload.Cmd.Flags, flags.API)
}

func TestParseLocatorsEmptyString(t *testing.T) {
	_, err := upload.ParseLocators("")
	assert.Error(t, err)
}

func TestParseLocatorsEmptyLine(t *testing.T) {
	locators := `fetcher+project$revision
fetcher+otherproject$revision

fetcher+lastproject$revision
`
	src, err := upload.ParseLocators(locators)
	assert.NoError(t, err)

	for _, dep := range src.Build.Dependencies {
		assert.NotNil(t, dep.Imports)
		assert.NotEmpty(t, dep.Locator)
	}
}
