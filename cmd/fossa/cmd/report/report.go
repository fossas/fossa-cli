package report

import (
	"github.com/urfave/cli"
)

// JSON flag for whether or not output should be json
const JSON = "json"

// Cmd command for generating reports
var Cmd = cli.Command{
	Name:  "report",
	Usage: "Generate reports",
	Subcommands: []cli.Command{
		dependenciesCmd,
		licensesCmd,
		attributionCmd,
	},
}
