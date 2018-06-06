package main

import (
	"os"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/build"
	initcmd "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
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
		// {
		// 	Name:   "analyze",
		// 	Usage:  "Analyze built dependencies",
		// 	Action: analyzeCmd,
		// 	Flags: []cli.Flag{
		// 		cli.StringFlag{Name: "c, config", Usage: configUsage},
		// 		cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
		// 		cli.StringFlag{Name: "p, project", Usage: projectUsage},
		// 		cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
		// 		cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
		// 		flags.Branch,
		// 		cli.StringSliceFlag{Name: "m, modules", Usage: "the modules to analyze"},
		// 		cli.BoolFlag{Name: "o, output", Usage: analyzeOutputUsage},
		// 		cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
		// 		cli.BoolFlag{Name: "debug", Usage: debugUsage},
		// 	},
		// },
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
		// upload.Cmd,
		// update.Cmd,
		// {
		// 	Name:   "report",
		// 	Usage:  "Generates a license report",
		// 	Action: reportCmd,
		// 	Flags: []cli.Flag{
		// 		cli.StringFlag{Name: "c, config", Usage: configUsage},
		// 		cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
		// 		cli.StringFlag{Name: "p, project", Usage: projectUsage},
		// 		cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
		// 		cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
		// 		cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
		// 		cli.StringFlag{Name: "t, type", Usage: "the type of report to generate (either \"dependencies\" or \"licenses\"", Value: "licenses"},
		// 		cli.BoolFlag{Name: "debug", Usage: debugUsage},
		// 	},
		// },
	}

	app.Run(os.Args)
}
