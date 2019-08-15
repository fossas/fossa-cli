package analyze

import (
	"fmt"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/module"
)

var NewCmd = cli.Command{
	Name:      "new-analyze",
	Usage:     "Analyze built dependencies",
	Action:    NewRun,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: "show-output, output, o", Usage: "print results to stdout instead of uploading to FOSSA"},
		flags.TemplateF,
	}))),
}

// TODO: progress indicators, parallelism, ...
func NewRun(ctx *cli.Context) error {
	err := setup.SetContext(ctx, !ctx.Bool(ShowOutput))
	if err != nil {
		log.Fatalf("Could not initialize %s", err)
		return err
	}

	// TODO: config
	// This will require:
	// - writing discovered strategies out to config (when a flag is set)
	//   use the DiscoverFunc on each analyzer to do this
	// - reading modules from config && bypassing discovery when a config file is present
	//   when bypassing: use AnalyzerV2.ScanModule(folder, strategies)
	// - incorporating vcs discovery and other configuration from v1

	res, ourErr := nodejs.NodeAnalyzer.AnalyzeV2()
	if ourErr != nil {
		log.Fatalf("Could not analyze modules: %s", ourErr.Error())
	}

	// TODO: remove the below -- it's all adapter/legacy code
	modules := module.ToModules(res)

	if len(modules) == 0 {
		log.Fatal("No modules found.") // TODO
	}

	log.Debugf("analyzed: %#v", modules)
	normalized, err := fossa.Normalize(modules)

	if err != nil {
		log.Fatalf("Could not normalize output: %s", err.Error())
		return err
	}

	if ctx.Bool(ShowOutput) {
		if tmplFile := ctx.String(flags.Template); tmplFile != "" {
			output, err := display.TemplateFile(tmplFile, normalized)
			fmt.Println(output)
			if err != nil {
				log.Fatalf("Could not parse template data: %s", err.Error())
			}
		} else {
			_, err := display.JSON(normalized)
			if err != nil {
				log.Fatalf("Could not serialize to JSON: %s", err.Error())
			}
		}

		return nil
	}

	// TODO: upload analysis
	return nil
}
