package cliutil

import (
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

func Init(ctx *cli.Context) config.CLIConfig {
	c := config.MustNew(ctx)
	log.Initialize(c.Interactive, c.Debug)
	fossa.Initialize(c.Endpoint, c.APIKey)
	return c
}
