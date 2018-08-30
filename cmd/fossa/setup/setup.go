// Package setup implements initialization for all application packages.
package setup

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
)

// SetContext initializes all application-level packages.
func SetContext(ctx *cli.Context) error {
	// Set up configuration.
	err := config.SetContext(ctx)
	if err != nil {
		return err
	}

	// Set up logging.
	display.SetInteractive(config.Interactive())
	display.SetDebug(config.Debug())

	// Set up API.
	err = fossa.SetEndpoint(config.Endpoint())
	if err != nil {
		return err
	}
	fossa.SetAPIKey(config.APIKey())

	return nil
}

// TODO: testing version of SetContext?
