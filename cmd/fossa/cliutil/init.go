package cliutil

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/services"
)

func MustInit(ctx *cli.Context) (services.Services, config.CLIConfig) {
	c := config.MustNew(ctx)
	io := services.New(c.Interactive, c.Debug)
	err := io.API.Initialize(c.Endpoint, c.APIKey)
	if err != nil {
		io.Logger.Fatalf("Could not initialize API: %s", err.Error())
	}

	return io, c
}
