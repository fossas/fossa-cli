package main

import (
	"os"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/build"
	initcmd "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/report"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/update"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

func main() {
	app := cli.App{
		Name:    "fossa-cli",
		Usage:   "Fast, portable and reliable dependency analysis (https://github.com/fossas/fossa-cli/)",
		Version: version.String(),
		Action:  cli.ShowAppHelp,
	}

	app.Commands = []cli.Command{
		initcmd.Cmd,
		build.Cmd,
		analyze.Cmd,
		upload.Cmd,
		report.Cmd,
		// {
		// 	Name:   "test",
		// 	Usage:  "Test current revision against FOSSA scan status and exit with errors if issues are found",
		// 	Action: testCmd,
		// 	Flags: []cli.Flag{
		// 		cli.StringFlag{Name: "c, config", Usage: configUsage},
		// 		cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
		// 		cli.StringFlag{Name: "p, project", Usage: projectUsage},
		// 		cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
		// 		cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
		// 		cli.IntFlag{Name: "t, timeout", Usage: "timeout for waiting for build status in seconds", Value: 60 * 10},
		// 		cli.BoolFlag{Name: "debug", Usage: debugUsage},
		// 	},
		// },
		update.Cmd,
	}

	app.Run(os.Args)
}
