package main

import (
	"fmt"
	"os"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/builders"
	initcmd "github.com/fossas/fossa-cli/cmd/fossa/cmd/init"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/update"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/upload"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

const (
	configUsage               = "path to config file (default: .fossa.{yml,yaml})"
	fetcherUsage              = "type of fetcher to use for fossa. Default's to custom"
	projectUsage              = "this repository's URL or VCS endpoint (default: VCS remote 'origin')"
	revisionUsage             = "this repository's current revision hash (default: VCS hash HEAD)"
	endpointUsage             = "the FOSSA server endpoint (default: https://app.fossa.io)"
	buildForceUsage           = "ignore cached build artifacts"
	analyzeOutputUsage        = "print results to stdout instead of uploading to FOSSA"
	analyzeAllowResolvedUsage = "allow unresolved dependencies"
	debugUsage                = "print debug information to stderr"
)

func main() {
	app := cli.App{
		Name:    "fossa-cli",
		Usage:   "Fast, portable and reliable dependency analysis (https://github.com/fossas/fossa-cli/)",
		Version: version.String(),
		Action:  cli.ShowAppHelp,
	}

	app.Commands = []cli.Command{
		initcmd.Cmd,
		{
			Name:   "build",
			Usage:  "Run a default project build",
			Action: buildCmd,
			Flags: []cli.Flag{
				// TODO: specify these using c.GlobalString?
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringSliceFlag{Name: "m, modules", Usage: "the modules to build"},
				cli.BoolFlag{Name: "f, force", Usage: buildForceUsage},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		{
			Name:   "analyze",
			Usage:  "Analyze built dependencies",
			Action: analyzeCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				flags.Branch,
				cli.StringSliceFlag{Name: "m, modules", Usage: "the modules to analyze"},
				cli.BoolFlag{Name: "o, output", Usage: analyzeOutputUsage},
				cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		{
			Name:   "test",
			Usage:  "Test current revision against FOSSA scan status and exit with errors if issues are found",
			Action: testCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				cli.IntFlag{Name: "t, timeout", Usage: "timeout for waiting for build status in seconds", Value: 60 * 10},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
		upload.Cmd,
		update.Cmd,
		{
			Name:   "report",
			Usage:  "Generates a license report",
			Action: reportCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "c, config", Usage: configUsage},
				cli.StringFlag{Name: "fetcher", Usage: fetcherUsage},
				cli.StringFlag{Name: "p, project", Usage: projectUsage},
				cli.StringFlag{Name: "r, revision", Usage: revisionUsage},
				cli.StringFlag{Name: "e, endpoint", Usage: endpointUsage},
				cli.BoolFlag{Name: "allow-unresolved", Usage: analyzeAllowResolvedUsage},
				cli.StringFlag{Name: "t, type", Usage: "the type of report to generate (either \"dependencies\" or \"licenses\"", Value: "licenses"},
				cli.BoolFlag{Name: "debug", Usage: debugUsage},
			},
		},
	}

	app.Run(os.Args)
}

func resolveModuleConfig(moduleConfig module.Config) (module.Builder, module.Module, error) {
	log.Logger.Debugf("Resolving moduleConfig: %#v", moduleConfig)

	var builder module.Builder
	var m module.Module

	moduleType, err := module.Parse(moduleConfig.Type)
	if err != nil {
		log.Logger.Debug("Got unknown module.")
		return builder, m, fmt.Errorf("unknown module type: %s", moduleConfig.Type)
	}

	log.Logger.Debugf("Got %s module.", moduleType)
	builder = builders.New(moduleType)

	if builder == nil {
		log.Logger.Debug("Got unknown builder.")
		return nil, m, fmt.Errorf("no builder available for type: %s", moduleConfig.Type)
	}

	m, err = module.New(moduleType, moduleConfig)
	if err != nil {
		log.Logger.Debug("Unable to resolve module config")
		return builder, m, fmt.Errorf("unable to setup module type: %s", moduleConfig.Type)
	}

	log.Logger.Debugf("Resolved moduleConfig to: %#v, %#v", builder, m)
	return builder, m, nil
}
