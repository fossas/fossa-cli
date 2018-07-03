package report

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Unknown  = "show-unknown"
	analyzed []module.Module
)

var Cmd = cli.Command{
	Name:  "report",
	Usage: "Generate reports",
	Subcommands: []cli.Command{
		dependenciesCmd,
		licensesCmd,
	},
}

func prepareReportCtx(ctx *cli.Context) (err error) {
	err = cmdutil.InitWithAPI(ctx)
	if err != nil {
		log.Logger.Fatalf("Could not initialize: %s", err.Error())
	}

	modules, err := config.Modules()
	if err != nil {
		log.Logger.Fatalf("Could not parse modules: %s", err.Error())
	}
	if len(modules) == 0 {
		log.Logger.Fatal("No modules specified.")
	}

	analyzed, err = analyze.Modules(modules)
	if err != nil {
		log.Logger.Fatalf("Could not analyze modules: %s", err.Error())
	}

	return nil
}
