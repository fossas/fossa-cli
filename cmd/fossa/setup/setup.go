// Package setup implements initialization for all application packages.
package setup

import (
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/config"
	"github.com/urfave/cli"
)

// SetContext initializes all application-level packages.
func SetContext(ctx *cli.Context, setAPIKey bool) error {
	// Set up configuration.
	err := config.SetContext(ctx)
	if err != nil {
		return err
	}

	// Set up logging.
	display.SetInteractive(config.Interactive())
	display.SetDebug(config.Debug(), config.Verbose())

	// Set up API.
	err = fossa.SetEndpoint(config.Endpoint())
	if err != nil {
		return err
	}

	if setAPIKey {
		err = fossa.SetAPIKey(config.APIKey())
		if err != nil {
			return err
		}
	}

	return nil
}

// TODO: testing version of SetContext?
