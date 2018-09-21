package main_test

import (
	"testing"

	main "github.com/fossas/fossa-cli/cmd/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/stretchr/testify/assert"
)

func TestMainProvidesDebugFlag(t *testing.T) {
	assert.Contains(t, main.App.VisibleFlags(), flags.DebugF)
}
