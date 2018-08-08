package report

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Unknown = "show-unknown"
)

var Cmd = cli.Command{
	Name:  "report",
	Usage: "Generate reports",
	Subcommands: []cli.Command{
		dependenciesCmd,
		licensesCmd,
	},
}

func analyzeModules(ctx *cli.Context) ([]module.Module, error) {
	err := setup.Setup(ctx)
	if err != nil {
		return nil, err
	}

	modules, err := config.Modules()
	if err != nil {
		return nil, err
	}
	if len(modules) == 0 {
		log.Logger.Fatal("No modules specified.")
	}

	analyzed, err := analyze.Do(modules)
	if err != nil {
		return nil, err
	}

	return analyzed, nil
}
