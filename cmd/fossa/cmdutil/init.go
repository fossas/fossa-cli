// Package cmdutil provides common utilities for subcommands.
package cmdutil

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

// Init initializes all application-level packages.
func Init(ctx *cli.Context) error {
	// We set these defaults first, otherwise the logging defaults will print
	// output as configuration is initialized.
	err := config.Init(ctx)
	if err != nil {
		return err
	}
	log.Init(config.Interactive(), config.Debug())
	return nil
}

func InitWithAPI(ctx *cli.Context) error {
	err := Init(ctx)
	if err != nil {
		return err
	}
	fossa.MustInit(config.Endpoint(), config.APIKey())
	return nil
}
