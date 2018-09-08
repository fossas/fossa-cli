package npm

import (
	"testing"

	"github.com/stretchr/testify/assert"
	// "github.com/fossas/fossa-cli/buildtools/npm/manifest"
)

func TestFromManifest(t *testing.T) {
	manifest, err := FromManifest("testdata/package.json")
	assert.NoError(t, err)

	assert.NotEmpty(t, manifest.Dependencies)
	assert.Equal(t, manifest.Dependencies["chai"], "4.1.2")
}
