package report

import (
	"github.com/urfave/cli"
)

const JSON = "json"

var Cmd = cli.Command{
	Name:  "report",
	Usage: "Generate reports",
	Subcommands: []cli.Command{
		dependenciesCmd,
		licensesCmd,
	},
}
