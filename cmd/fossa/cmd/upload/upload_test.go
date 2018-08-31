package upload_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestUploadHasEndpointFlag(t *testing.T) {
	assert.Subset(t, upload.Cmd.Flags, flags.API)
}
