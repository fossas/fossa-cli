package config_test

import (
	"flag"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
)

func TestInitWorksWithoutGitRepository(t *testing.T) {
	err := os.Chdir("/")
	assert.NoError(t, err)

	ctx := cli.NewContext(cli.NewApp(), flag.NewFlagSet("test", 0), nil)
	err = config.Init(ctx)
	assert.NoError(t, err)
}
