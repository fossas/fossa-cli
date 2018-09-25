package main_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	main "github.com/fossas/fossa-cli/cmd/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestMainProvidesDebugFlag(t *testing.T) {
	assert.Contains(t, main.App.VisibleFlags(), flags.DebugF)
}
