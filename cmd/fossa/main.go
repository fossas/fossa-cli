package main

import (
	"fmt"
	"os"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/build"
	initc "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/report"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/test"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/update"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

var App = cli.App{
	Name:                 "fossa-cli",
	Usage:                "Fast, portable and reliable dependency analysis (https://github.com/fossas/fossa-cli/)",
	Version:              version.String(),
	Action:               Run,
	EnableBashCompletion: true,
	Flags: flags.Combine(
		initc.Cmd.Flags,
		analyze.Cmd.Flags,
		flags.WithGlobalFlags(nil),
	),
	Commands: []cli.Command{
		initc.Cmd,
		build.Cmd,
		analyze.Cmd,
		upload.Cmd,
		report.Cmd,
		test.Cmd,
		update.Cmd,
		analyze.NewCmd,
	},
}

func main() {
	// Write the temp log file to disk after the command finishes.
	defer func() {
		err := display.SyncFile()
		if err != nil {
			log.Warnf("error writing to the log file: %s", err.Error())
		}
	}()

	defer display.SyncFile()

	err := App.Run(os.Args)
	if err != nil {
		switch e := err.(type) {
		case *cli.ExitError:
			os.Exit(e.ExitCode())
		default:
			fmt.Println(err.Error())
			os.Exit(1)
		}
	}
}

func Run(ctx *cli.Context) error {
	err := setup.SetContext(ctx, true)
	if err != nil {
		return err
	}

	err = initc.Run(ctx)
	if err != nil {
		return err
	}

	err = analyze.Run(ctx)
	if err != nil {
		return err
	}

	return nil
}
