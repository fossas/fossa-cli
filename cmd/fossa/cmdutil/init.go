package cmdutil

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
)

func Init(ctx *cli.Context) {
	config.Init(ctx)
	log.Init(config.Interactive(), config.Debug())
}

func InitWithAPI(ctx *cli.Context) {
	Init(ctx)
	fossa.MustInit(config.Endpoint(), config.APIKey())
}
