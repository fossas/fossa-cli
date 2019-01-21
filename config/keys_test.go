package config_test

import (
	"flag"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/pkg"
)

func TestModulesCommandLineOptions(t *testing.T) {
	// Set the expected flags.
	flagSet := flag.NewFlagSet("test", 0)
	flags.ConfigF.Apply(flagSet)
	flags.OptionF.Apply(flagSet)

	// Set the flag values.
	ctx := cli.NewContext(cli.NewApp(), flagSet, nil)
	err := ctx.Set(flags.Config, "testdata/test.yml")
	assert.NoError(t, err)
	err = ctx.Set(flags.Option, "allow-unresolved:true")
	assert.NoError(t, err)

	// Finalize the context.
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

	// Test that command line options are preferred when they conflict config options.
	assert.Equal(t, 1, len(modules[2].Options))
	assert.Equal(t, true, modules[2].Options["allow-unresolved"])
}

func TestModulesWithAnArgumentAndOptions(t *testing.T) {
	// Set the expected flags.
	flagSet := flag.NewFlagSet("test", 0)
	flags.ConfigF.Apply(flagSet)
	flags.OptionF.Apply(flagSet)
	err := flagSet.Parse([]string{"gradle:argumentmodule"})
	assert.NoError(t, err)

	// Set the flag values.
	ctx := cli.NewContext(cli.NewApp(), flagSet, nil)
	err = ctx.Set(flags.Config, "testdata/test.yml")
	assert.NoError(t, err)
	err = ctx.Set(flags.Option, "allsubmodules:true")
	assert.NoError(t, err)

	// Finalize the context.
	err = config.SetContext(ctx)
	assert.NoError(t, err)

	// Assert that only argument modules are detected, not conifg modules.
	modules, err := config.Modules()
	assert.NoError(t, err)
	assert.Equal(t, modules[0].Type, pkg.Gradle)
	assert.Equal(t, modules[0].BuildTarget, "argumentmodule")
	assert.Len(t, modules, 1)

	// Test that command line options are set and no other options exist.
	assert.Equal(t, 1, len(modules[0].Options))
	assert.Equal(t, true, modules[0].Options["allsubmodules"])
}

func TestNoModulesError(t *testing.T) {
	// Set the expected flags.
	flagSet := flag.NewFlagSet("test", 0)
	flags.ConfigF.Apply(flagSet)

	// Set the flag values.
	ctx := cli.NewContext(cli.NewApp(), flagSet, nil)
	err := ctx.Set(flags.Config, "testdata/test_no_modules.yml")
	assert.NoError(t, err)

	// Finalize the context.
	err = config.SetContext(ctx)
	assert.NoError(t, err)

	modules, err := config.Modules()
	assert.EqualError(t, err, "No modules provided")
	assert.Equal(t, 0, len(modules))
}
