package main

import (
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
)

var App = cli.App{
	Name:    "fossa-cli",
	Usage:   "Fast, portable and reliable dependency analysis (https://github.com/fossas/fossa-cli/)",
	Version: version.String(),
	Action:  defaultCmd,
	Flags: flags.WithAPIFlags(flags.WithGlobalFlags([]cli.Flag{
		flags.Config,
	})),
}
