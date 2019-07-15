package flags

import (
	"fmt"
	"strings"

	"github.com/urfave/cli"
)

func Combine(sets ...[]cli.Flag) []cli.Flag {
	flags := make(map[string]cli.Flag)
	for _, s := range sets {
		for _, f := range s {
			name := f.GetName()
			prev, ok := flags[name]
			if ok {
				if prev == f {
					continue
				} else {
					panic(f)
				}
			} else {
				flags[name] = f
			}
		}
	}

	var combined []cli.Flag
	for _, f := range flags {
		combined = append(combined, f)
	}
	return combined
}

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
	API             = []cli.Flag{EndpointF, TitleF, FetcherF, ProjectF, RevisionF, BranchF, ProjectURLF, JIRAProjectKeyF, LinkF, TeamF}
	Endpoint        = "endpoint"
	EndpointF       = cli.StringFlag{Name: Short(Endpoint), Usage: "the FOSSA server endpoint (default: 'https://app.fossa.com')"}
	Title           = "title"
	TitleF          = cli.StringFlag{Name: Short(Title), Usage: "the title of the FOSSA project. (default: the project name)"}
	Fetcher         = "fetcher"
	FetcherF        = cli.StringFlag{Name: Short(Fetcher), Usage: "type of fetcher to use for fossa. (default: 'custom')"}
	Project         = "project"
	ProjectF        = cli.StringFlag{Name: Short(Project), Usage: "this repository's URL or VCS endpoint (default: VCS remote 'origin')"}
	Revision        = "revision"
	RevisionF       = cli.StringFlag{Name: Short(Revision), Usage: "this repository's current revision hash (default: VCS hash HEAD)"}
	Branch          = "branch"
	BranchF         = cli.StringFlag{Name: Short(Branch), Usage: "this repository's current branch (default: current VCS branch)"}
	ProjectURL      = "project-url"
	ProjectURLF     = cli.StringFlag{Name: ShortUpper(ProjectURL), Usage: "this repository's home page"}
	JIRAProjectKey  = "jira-project-key"
	JIRAProjectKeyF = cli.StringFlag{Name: Short(JIRAProjectKey), Usage: "this repository's JIRA project key"}
	Link            = "link"
	LinkF           = cli.StringFlag{Name: ShortUpper(Link), Usage: "a link to attach to the current build"}
	Team            = "team"
	TeamF           = cli.StringFlag{Name: ShortUpper(Team), Usage: "this repository's team inside your organization"}
)

func WithGlobalFlags(f []cli.Flag) []cli.Flag {
	return append(f, Global...)
}

var (
	Global        = []cli.Flag{ConfigF, NoAnsiF, DebugF, DebugCallersF}
	Config        = "config"
	ConfigF       = cli.StringFlag{Name: Short(Config), Usage: "path to config file (default: '.fossa.{yml,yaml}')"}
	NoAnsi        = "no-ansi"
	NoAnsiF       = cli.BoolFlag{Name: NoAnsi, Usage: "do not use interactive mode (ANSI codes)"}
	Debug         = "debug"
	DebugF        = cli.BoolFlag{Name: Debug, Usage: "print debug information to stderr"}
	DebugCallers  = "debug-callers"
	DebugCallersF = cli.BoolFlag{Name: DebugCallers, Usage: "print debug information with full call stack to stderr"}
)

func WithOptions(f []cli.Flag) []cli.Flag {
	return append(f, Options...)
}

var (
	Options = []cli.Flag{OptionF}
	Option  = "option"
	OptionF = cli.StringSliceFlag{Name: Option, Usage: "set configurable options (format is `key:value` e.g. allow-unresolved:true)"}
)

func WithReportTemplateFlags(f []cli.Flag) []cli.Flag {
	return append(f, ReportCmd...)
}

var (
	ReportCmd   = []cli.Flag{OutputFileF, TemplateF}
	OutputFile  = "output-file"
	OutputFileF = cli.StringFlag{Name: OutputFile, Value: "-", Usage: "Output file for report"}
	Template    = "template"
	TemplateF   = cli.StringFlag{Name: Template, Usage: "process result via template file prior to sending it to output"}
)
