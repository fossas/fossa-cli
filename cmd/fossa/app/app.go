package app

import (
	"fmt"
	"log"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/build"
	initc "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/report"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/test"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/update"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
	"github.com/fossas/fossa-cli/config"
)

func New() *cli.App {
	return &cli.App{
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
		},
	}
}

func Run(ctx *cli.Context) error {
	err := setup.SetContext(ctx)
	if err != nil {
		return err
	}

	if config.APIKey() == "" && !ctx.Bool(analyze.ShowOutput) {
		fmt.Printf("Incorrect Usage. FOSSA_API_KEY must be set as an environment variable or provided in .fossa.yml\n\n")
		log.Fatalf("No API KEY provided")
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
