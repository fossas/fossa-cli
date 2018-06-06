// Package init implements `fossa init`.
package init

import (
	"regexp"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/pkg"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Overwrite  = "overwrite"
	IncludeAll = "include-all"
)

// Cmd exports the `init` CLI command.
var Cmd = cli.Command{
	Name:   "init",
	Usage:  "Initialize a .fossa.yml configuration file",
	Action: Run,
	Flags: flags.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: flags.ShortUpper(Overwrite), Usage: "overwrite modules in config even if they exist"},
		cli.BoolFlag{Name: IncludeAll, Usage: "include suspicious modules (e.g. `docs`, `test` or `example` in name)"},
	}),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := cmdutil.Init(ctx)
	if err != nil {
		log.Logger.Fatalf("Could not initialize: %s", err.Error())
	}
	modules, err := Do(ctx.Bool(Overwrite), ctx.Bool(IncludeAll))
	if err != nil {
		log.Logger.Fatalf("Could not run init: %s", err.Error())
	}
	err = config.InitFile(modules)
	if err != nil {
		log.Logger.Fatalf("Could not write config: %s", err.Error())
	}
	return nil
}

// Do discovers modules within the current working directory.
func Do(overwrite, includeAll bool) ([]module.Module, error) {
	defer log.StopSpinner()
	log.ShowSpinner("Initializing...")

	// Discover all modules.
	var discovered []module.Module
	for _, t := range pkg.AllTypes {
		analyzer, err := analyzers.New(t, nil)
		if err != nil {
			log.Logger.Warningf("Could not initialize analyzer for type %s: %s", t.String(), err.Error())
			continue
		}
		modules, err := analyzer.Discover(".")
		if err != nil {
			log.Logger.Warningf("Discovery failed for analyzer %s: %s", t.String(), err.Error())
		}
		discovered = append(discovered, modules...)
	}

	// Filter suspicious modules.
	if includeAll {
		return discovered, nil
	}
	var filtered []module.Module
	for _, d := range discovered {
		matched, err := regexp.MatchString("(docs?/|test|example|vendor/|node_modules/|.srclib-cache/|spec/|Godeps/|.git/|bower_components/|third_party/)", d.Dir)
		if err != nil {
			return nil, err
		}
		if matched {
			log.Logger.Warningf("Filtering out suspicious module: %s (%s)", d.Name, d.BuildTarget)
		} else {
			filtered = append(filtered, d)
		}
	}
	return filtered, nil
}
