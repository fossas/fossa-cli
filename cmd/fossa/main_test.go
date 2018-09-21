package main_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/fossas/fossa-cli/cmd/fossa/app"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestMainProvidesDebugFlag(t *testing.T) {
	assert.Contains(t, app.App.VisibleFlags(), flags.DebugF)
}
