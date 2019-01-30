package paket_test

import (
	"testing"

	"github.com/fossas/fossa-cli/buildtools/paket"
)

func TestPaketLockParsing(t *testing.T) {
	paket.DependencyGraph("testdata/paket.lock")
	/* 	assert.NoError(t, err)
	   	assert.NotEmpty(t, deps) */
}
