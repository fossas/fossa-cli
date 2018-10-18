package config_test

import (
	"flag"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/vcs"
)

func TestInitWorksWithoutGitRepository(t *testing.T) {
	// Change to a directory without Git repository.
	cwd, err := os.Getwd()
	assert.NoError(t, err)
	err = os.Chdir("/")
	assert.NoError(t, err)
	defer func() {
		os.Chdir(cwd)
	}()

	// Check that there actually is no Git repository.
	_, _, err = vcs.Nearest(".")
	assert.Error(t, err, vcs.ErrNoNearestVCS)

	// Check that config.setContext doesn't break.
	ctx := cli.NewContext(cli.NewApp(), flag.NewFlagSet("test", 0), nil)
	err = config.SetContext(ctx)
	assert.NoError(t, err)
}
