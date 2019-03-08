package report

import (
	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
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
	err := setup.SetContext(ctx)
	if err != nil {
		return nil, err
	}

	modules, err := config.Modules()
	if err != nil {
		return nil, err
	}
	if len(modules) == 0 {
		log.Fatal("No modules specified.")
	}

	analyzed, err := analyze.Do(modules, false)
	if err != nil {
		return nil, err
	}

	return analyzed, nil
}
