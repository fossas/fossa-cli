package config_test

import (
	"flag"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
)

func TestModulesCommandLineOptions(t *testing.T) {
	// Set the expected flags
	flagSet := flag.NewFlagSet("test", 0)
	flags.ConfigF.Apply(flagSet)
	flags.OptionF.Apply(flagSet)

	// Set the flag values
	ctx := cli.NewContext(cli.NewApp(), flagSet, nil)
	err := ctx.Set(flags.Config, "testdata/test.yml")
	assert.NoError(t, err)
	err = ctx.Set(flags.Option, "allow-unresolved:true")
	assert.NoError(t, err)

	//Finalize the context
	err = config.SetContext(ctx)
	assert.NoError(t, err)

	modules, err := config.Modules()
	assert.NoError(t, err)

	// Test that command line options are set.
	assert.Equal(t, 1, len(modules[0].Options))
	assert.Equal(t, true, modules[0].Options["allow-unresolved"])

	// Test that config file options are not overwritten.
	assert.Equal(t, 2, len(modules[1].Options))
	assert.Equal(t, true, modules[1].Options["allow-unresolved"])
	assert.Equal(t, "linux", modules[1].Options["os"])

	// Test that command line options are preferred when they conflict.
	assert.Equal(t, 1, len(modules[2].Options))
	assert.Equal(t, true, modules[2].Options["allow-unresolved"])
}
