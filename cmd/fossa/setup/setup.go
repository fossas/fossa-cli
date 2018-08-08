package setup

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
)

func Setup(ctx *cli.Context) error {
	// We set these defaults first, otherwise the logging defaults will print
	// output as configuration is initialized.
	err := config.Init(ctx)
	if err != nil {
		return err
	}

	// Set up logging.
	display.Init(config.Interactive(), config.Debug())

	// Set up API.
	return fossa.Init(config.Endpoint(), config.APIKey())
}
