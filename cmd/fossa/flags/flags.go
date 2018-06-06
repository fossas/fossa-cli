package flags

import (
	"fmt"
	"strings"

	"github.com/urfave/cli"
)

func Short(name string) string {
	return fmt.Sprintf("%c, %s", name[0], name)
}

func ShortUpper(name string) string {
	return fmt.Sprintf("%s, %s", strings.ToUpper(name[:1]), name)
}

func WithAPIFlags(f []cli.Flag) []cli.Flag {
	return append(f, API...)
}

var (
	API       = []cli.Flag{EndpointF, FetcherF, ProjectF, RevisionF, BranchF}
	Endpoint  = "endpoint"
	EndpointF = cli.StringFlag{Name: Short(Endpoint), Usage: "the FOSSA server endpoint (default: 'https://app.fossa.io')"}
	Fetcher   = "fetcher"
	FetcherF  = cli.StringFlag{Name: Short(Fetcher), Usage: "type of fetcher to use for fossa. (default: 'custom')"}
	Project   = "project"
	ProjectF  = cli.StringFlag{Name: Short(Project), Usage: "this repository's URL or VCS endpoint (default: VCS remote 'origin')"}
	Revision  = "revision"
	RevisionF = cli.StringFlag{Name: Short(Revision), Usage: "this repository's current revision hash (default: VCS hash HEAD)"}
	Branch    = "branch"
	BranchF   = cli.StringFlag{Name: Short(Branch), Usage: "this repository's current branch (default: current VCS branch)"}
)

func WithGlobalFlags(f []cli.Flag) []cli.Flag {
	return append(f, Global...)
}

var (
	Global  = []cli.Flag{ConfigF, NoAnsiF, DebugF}
	Config  = "config"
	ConfigF = cli.StringFlag{Name: Short(Config), Usage: "path to config file (default: '.fossa.{yml,yaml}')"}
	NoAnsi  = "no-ansi"
	NoAnsiF = cli.BoolFlag{Name: NoAnsi, Usage: "do not use interactive mode (ANSI codes)"}
	Debug   = "debug"
	DebugF  = cli.BoolFlag{Name: Debug, Usage: "print debug information to stderr"}
)

func WithModulesFlags(f []cli.Flag) []cli.Flag {
	return append(f, Modules...)
}

var (
	Modules = []cli.Flag{OptionF}
	Option  = "option"
	OptionF = cli.StringSliceFlag{Name: Option, Usage: "options for the module (format is `key:value` e.g. allow-unresolved:true)"}
)

var (
	AnalysisCmd = []cli.Flag{ShowOutputF}
	ShowOutput  = "output"
	ShowOutputF = cli.BoolFlag{Name: Short(ShowOutput), Usage: "print results to stdout instead of uploading to FOSSA"}
)
