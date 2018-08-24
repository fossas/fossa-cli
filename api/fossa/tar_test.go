package fossa_test

import (
	"encoding/base64"
	"log"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/api/fossa"
)

func TestCreateTar(t *testing.T) {
	tmp, hash, err := fossa.CreateTarball("testdata")
	assert.NoError(t, err)
	log.Printf("Hash: %#v", hash)

	// The file matching this hash has been manually checked to make sure it's a
	// correct, readable tarball with the correct contents.
	assert.Equal(t, "5fRhTw+40JqCpCoJuph+hw==", base64.StdEncoding.EncodeToString(hash))

	if os.Getenv("KEEP_TMP") == "" {
		err = os.Remove(tmp.Name())
		assert.NoError(t, err)
	} else {
		log.Printf("Generated tarball at: %#v", tmp.Name())
	}
}
