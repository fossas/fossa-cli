// Package init implements `fossa init`.
package init

import (
	"regexp"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/pkg"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Update     = "update"
	IncludeAll = "include-all"
	Type       = "type"
)

// Cmd exports the `init` CLI command.
var Cmd = cli.Command{
	Name:   "init",
	Usage:  "Initialize a .fossa.yml configuration file",
	Action: Run,
	Flags: flags.WithGlobalFlags(flags.WithOptions([]cli.Flag{
		// cli.BoolFlag{Name: Update, Usage: "update an existing configuration file"},
		cli.BoolFlag{Name: IncludeAll, Usage: "include suspicious modules (e.g. `docs`, `test` or `example` in name)"},
		cli.StringSliceFlag{Name: Type, Usage: "the module types to check for"},
	})),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.Setup(ctx)
	if err != nil {
		log.Logger.Fatalf("Could not initialize: %s", err.Error())
	}

	hasConfigFile, err := config.ExistsFile()
	if err != nil {
		log.Logger.Fatalf("Could not detect configuration file: %s", err.Error())
	}

	if !hasConfigFile || ctx.Bool(Update) {
		options, err := config.Options()
		if err != nil {
			log.Logger.Fatalf("Could not parse options: %s", err.Error())
		}
		modules, err := Do(ctx.Bool(IncludeAll), options)
		if err != nil {
			log.Logger.Fatalf("Could not run init: %s", err.Error())
		}
		err = config.WriteFile(modules)
		if err != nil {
			log.Logger.Fatalf("Could not write config: %s", err.Error())
		}
	} else {
		log.Logger.Warning("Existing configuration available; skipping initialization")
	}
	return nil
}

// Do discovers modules within the current working directory.
func Do(includeAll bool, options map[string]interface{}) ([]module.Module, error) {
	defer log.StopSpinner()
	log.ShowSpinner("Initializing...")

	// Discover all modules.
	discovered, err := analyzers.Discover(".", options)
	if err != nil {
		log.Logger.Warningf("An error occurred during discovery: %s", err.Error())
	}

	// TODO: Check whether modules were previously ignored.

	// Filter noisy modules (docs, examples, etc.).
	if includeAll {
		return discovered, nil
	}
	var filtered []module.Module
	for _, d := range discovered {
		log.Logger.Debugf("Discovered: %#v", d)

		// Match name regexp.
		// TODO: should we match on full path sections (e.g. right now, this will filter out `a/b/foovendor/c`)?
		matched, err := regexp.MatchString("(docs?/|[Tt]est|examples?|vendor/|node_modules/|.srclib-cache/|spec/|Godeps/|.git/|bower_components/|third_party/|tmp/|Carthage/Checkouts/)", d.Dir)
		if err != nil {
			return nil, err
		}
		if matched {
			log.Logger.Warningf("Filtering out suspicious module: %s (%s)", d.Name, d.BuildTarget)
			continue
		}

		// For Go, filter out non-executable packages.
		if d.Type == pkg.Go && !d.IsExecutable {
			continue
		}

		filtered = append(filtered, d)
	}
	return filtered, nil
}
