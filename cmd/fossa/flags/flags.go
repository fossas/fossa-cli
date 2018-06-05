package flags

import (
	"fmt"

	"github.com/urfave/cli"
)

func abbr(fullname string) string {
	return fmt.Sprintf("%s, %s", fullname[0], fullname)
}

func WithAPIFlags(f []cli.Flag) []cli.Flag {
	return append(f, API...)
}

var (
	API              = []cli.Flag{Endpoint, Fetcher, Project, Revision, Branch}
	EndpointFlagName = "endpoint"
	Endpoint         = cli.StringFlag{Name: abbr(EndpointFlagName), Usage: "the FOSSA server endpoint (default: 'https://app.fossa.io')"}
	FetcherFlagName  = "fetcher"
	Fetcher          = cli.StringFlag{Name: abbr(FetcherFlagName), Usage: "type of fetcher to use for fossa. (default: 'custom')"}
	ProjectFlagName  = "project"
	Project          = cli.StringFlag{Name: abbr(ProjectFlagName), Usage: "this repository's URL or VCS endpoint (default: VCS remote 'origin')"}
	RevisionFlagName = "revision"
	Revision         = cli.StringFlag{Name: abbr(RevisionFlagName), Usage: "this repository's current revision hash (default: VCS hash HEAD)"}
	BranchFlagName   = "branch"
	Branch           = cli.StringFlag{Name: abbr(BranchFlagName), Usage: "this repository's current branch (default: current VCS branch)"}
)

func WithGlobalFlags(f []cli.Flag) []cli.Flag {
	return append(f, Global...)
}

var (
	Global         = []cli.Flag{Config, NoAnsi, Debug}
	ConfigFlagName = "config"
	Config         = cli.StringFlag{Name: abbr(ConfigFlagName), Usage: "path to config file (default: '.fossa.{yml,yaml}')"}
	NoAnsiFlagName = "no-ansi"
	NoAnsi         = cli.BoolFlag{Name: NoAnsiFlagName, Usage: "do not use interactive mode (ANSI codes)"}
	DebugFlagName  = "debug"
	Debug          = cli.BoolFlag{Name: DebugFlagName, Usage: "print debug information to stderr"}
)

var (
	AnalysisShowOutput = cli.BoolFlag{Name: "o, output", Usage: "print results to stdout instead of uploading to FOSSA"}
)
