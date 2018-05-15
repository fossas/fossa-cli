package flags

import "github.com/urfave/cli"

var Config = cli.StringFlag{Name: "c, config", Usage: "path to config file (default: '.fossa.{yml,yaml}')"}

func WithAPIFlags(f []cli.Flag) []cli.Flag {
	return append(f, API...)
}

var (
	API      = []cli.Flag{Endpoint, Fetcher, Project, Revision}
	Endpoint = cli.StringFlag{Name: "e, endpoint", Usage: "the FOSSA server endpoint (default: 'https://app.fossa.io')"}
	Fetcher  = cli.StringFlag{Name: "f, fetcher", Usage: "type of fetcher to use for fossa. (default: 'custom')"}
	Project  = cli.StringFlag{Name: "p, project", Usage: "this repository's URL or VCS endpoint (default: VCS remote 'origin')"}
	Revision = cli.StringFlag{Name: "r, revision", Usage: "this repository's current revision hash (default: VCS hash HEAD)"}
)

func WithGlobalFlags(f []cli.Flag) []cli.Flag {
	return append(f, Global...)
}

var (
	Global = []cli.Flag{NoAnsi, Debug}
	NoAnsi = cli.BoolFlag{Name: "no-ansi", Usage: "do not use interactive mode (ANSI codes)"}
	Debug  = cli.BoolFlag{Name: "debug", Usage: "print debug information to stderr"}
)
