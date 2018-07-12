package main

import (
	"os"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/version"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/build"
	initc "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/report"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/update"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
)

func main() {
	app := cli.App{
		Name:    "fossa-cli",
		Usage:   "Fast, portable and reliable dependency analysis (https://github.com/fossas/fossa-cli/)",
		Version: version.String(),
		Action:  cli.ShowAppHelp,
	}

	app.Commands = []cli.Command{
		initc.Cmd,
		build.Cmd,
		analyze.Cmd,
		upload.Cmd,
		report.Cmd,
		// test.Cmd,
		update.Cmd,
	}

	app.Run(os.Args)
}
