package cliutil

import (
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

func Initialize(ctx *cli.Context, useAPI bool) config.CLIConfig {
	c := config.MustNew(ctx)
	log.Initialize(c.Interactive, c.Debug)
	if useAPI {
		fossa.MustInitialize(c.Endpoint, c.APIKey)
	}
	return c
}
