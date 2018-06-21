package analyze

import (
	"encoding/json"
	"fmt"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Output = "output"
)

var Cmd = cli.Command{
	Name:      "analyze",
	Usage:     "Analyze built dependencies",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithModulesFlags([]cli.Flag{
		cli.BoolFlag{Name: flags.Short(Output), Usage: "show analysis output instead of uploading to FOSSA"},
	}))),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := cmdutil.Init(ctx)
	if err != nil {
		log.Logger.Fatalf("Could not initialize: %s", err.Error())
	}

	modules, err := config.Modules()
	if err != nil {
		log.Logger.Fatalf("Could not parse modules: %s", err.Error())
	}
	if len(modules) == 0 {
		log.Logger.Fatal("No modules specified.")
	}

	var analyzed []module.Module
	defer log.StopSpinner()
	for i, m := range modules {
		log.ShowSpinner(fmt.Sprintf("Analyzing module (%d/%d): %s", i+1, len(modules), m.Name))
		analyzer, err := analyzers.New(m.Type, m.Options)
		if err != nil {
			log.Logger.Warningf("Could not load analyzer: %s", err.Error())
		}
		result, err := analyzer.Analyze(m)
		if err != nil {
			log.Logger.Fatalf("Could not analyze: %s", err.Error())
		}
		analyzed = append(analyzed, result)
	}
	log.StopSpinner()

	normalized, err := fossa.Normalize(analyzed)
	if err != nil {
		log.Logger.Fatalf("Could not normalize output: %s", err.Error())
	}
	if ctx.Bool(Output) {
		out, err := json.Marshal(normalized)
		if err != nil {
			log.Logger.Fatalf("Could not marshal output: %s", err.Error())
		}
		log.Printf("%s", string(out))
		return nil
	}

	fossa.MustInit(config.Endpoint(), config.APIKey())
	log.ShowSpinner("Uploading analysis...")
	locator, err := fossa.Upload(config.Fetcher(), config.Project(), config.Revision(), config.Title(), config.Branch(), normalized)
	if err != nil {
		log.Logger.Fatalf("Error during upload: %s", err.Error())
	}
	log.StopSpinner()
	log.Printf(cmdutil.FmtReportURL(locator))
	return nil
}
