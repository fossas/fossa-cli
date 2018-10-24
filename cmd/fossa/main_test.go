package main_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	main "github.com/fossas/fossa-cli/cmd/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
)

func TestMainProvidesDebugFlag(t *testing.T) {
	assert.Contains(t, main.App.VisibleFlags(), flags.DebugF)
}

func TestNoDuplicateFlags(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			if strings.HasPrefix(r.(string), "upload flag redefined") {
				assert.Fail(t, "duplicate flag detected", r)
			} else {
				panic(r)
			}
		}
	}()
	_ = main.App.Run([]string{"fossa", "upload"})
}
