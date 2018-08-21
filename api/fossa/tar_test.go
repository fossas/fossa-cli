package fossa_test

import (
	"encoding/base64"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/api/fossa"
)

func TestCreateTar(t *testing.T) {
	tmp, hash, err := fossa.CreateTarball("testdata")
	assert.NoError(t, err)

	// The file matching this hash has been manually checked to make sure it's a
	// correct, readable tarball with the correct contents.
	assert.Equal(t, "NLa6Blne2KpQbgevwB3R+A==", base64.StdEncoding.EncodeToString(hash))

	err = os.Remove(tmp.Name())
	assert.NoError(t, err)
}
