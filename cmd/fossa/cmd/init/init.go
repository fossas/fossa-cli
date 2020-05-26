// Package init implements `fossa init`.
package init

import (
	"regexp"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
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
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: IncludeAll, Usage: "include suspicious modules (e.g. `docs`, `test` or `example` in name)"},
		cli.StringSliceFlag{Name: Type, Usage: "the module types to check for"},
	}))),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.SetContext(ctx, false)
	if err != nil {
		log.Fatalf("Could not initialize: %s", err.Error())
	}

	hasConfigFile, err := config.ExistsFile()
	if err != nil {
		log.Fatalf("Could not detect configuration file: %s", err.Error())
	}

	if !hasConfigFile || ctx.Bool(Update) {
		options, err := config.Options()
		if err != nil {
			log.Fatalf("Could not parse options: %s", err.Error())
		}
		modules, err := Do(ctx.Bool(IncludeAll), options)
		if err != nil {
			log.Fatalf("Could not run init: %s", err.Error())
		}
		err = config.WriteFile(modules)
		if err != nil {
			log.Fatalf("Could not write config: %s", err.Error())
		}
	} else {
		log.Warn("Existing configuration available; skipping initialization")
	}
	return nil
}

// Do discovers modules within the current working directory.
func Do(includeAll bool, options map[string]interface{}) ([]module.Module, error) {
	defer display.ClearProgress()
	display.InProgress("Initializing...")

	// Discover all modules.
	discovered, err := analyzers.Discover(".", options)
	if err != nil {
		log.Warnf("An error occurred during discovery: %s", err.Error())
	}

	// TODO: Check whether modules were previously ignored.

	if includeAll {
		return discovered, nil
	}

	// Filter noisy modules (docs, examples, etc.).
	return FilterSuspiciousModules(discovered), nil
}

// FilterSuspiciousModules removes modules from known paths to prevent vendored third-party,
// test, or other modules that likely do not belong to the first party project being analyzed.
func FilterSuspiciousModules(modules []module.Module) []module.Module {
	var filtered []module.Module
	suspicious := regexp.MustCompile(`docs?[/\\]|[Tt]est|examples?|vendor[/\\]|site-packages[/\\]|node_modules[/\\]|.srclib-cache[/\\]|spec[/\\]|Godeps[/\\]|.git[/\\]|bower_components[/\\]|third[_-]party[/\\]|tmp[/\\]|Carthage[/\\]Checkouts[/\\]`)
	for _, d := range modules {
		log.Debugf("Discovered: %#v", d)

		// Match name regexp.
		// TODO: should we match on full path sections (e.g. right now, this will filter out `a/b/foovendor/c`)?
		matched := suspicious.MatchString(d.Dir)
		if matched {
			log.Warnf("Filtering out suspicious module: %s (%s)", d.Name, d.BuildTarget)
			continue
		}

		// For Go, filter out non-executable packages.
		if d.Type == pkg.Go && !d.IsExecutable {
			continue
		}

		filtered = append(filtered, d)
	}
	return filtered
}
