package main

import (
	"os"

	"github.com/fossas/fossa-cli/cmd/fossa/app"

	"github.com/apex/log"
	"github.com/urfave/cli"
)

func main() {
	err := app.New().Run(os.Args)
	if err != nil {
		switch e := err.(type) {
		case *cli.ExitError:
			os.Exit(e.ExitCode())
		default:
			// TODO: port all log.Fatal to instead return an error.
			log.Debugf("Error: %#v", err.Error())
			os.Exit(1)
		}
	}
}
