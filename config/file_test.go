package config_test

import (
	"testing"

	"github.com/fossas/fossa-cli/config"
	"github.com/stretchr/testify/assert"
)

func TestInitFileDoesNotWriteBranch(t *testing.T) {
	config.MockBranch = "not-empty"
	file := config.InitFile(nil)

	assert.Empty(t, file.Branch())
}
