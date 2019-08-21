package analyze

import (
	"encoding/json"
	"fmt"
	"github.com/fossas/fossa-cli/api/fossa"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/analyzers/nodejs"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/module"
	"github.com/urfave/cli"
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

var Analyzers = []module.AnalyzerV2{
	nodejs.NodeAnalyzer,
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
	//   when reading: sanity-check strategy names
	// - incorporating vcs discovery and other configuration from v1

	res := NewDo()

	// TODO: remove the below -- just for demo / dev purposes

	marshalled, _ := json.Marshal(fossa.ApiFormatModules(res))
	fmt.Println(string(marshalled))

	// TODO: normalize, show output (flag), upload
	return nil
}

func NewDo() map[module.Filepath][]module.Analysis {
	results := make(map[module.Filepath][]module.Analysis)

	for _, analyzer := range Analyzers {
		modules, err := analyzer.DiscoverFunc(".") // TODO: hardcoded "."
		if err != nil {
			log.Debugf("%s: Module discovery failed: %s", analyzer.Name, err.Error())
			continue
		}

		for modulePath, strategies := range modules {
			scanned, err := analyzer.ScanModule(modulePath, strategies)
			if err != nil {
				// TODO: use better errors
				log.Errorf("%s: Module scanning at %s failed: %s", analyzer.Name, modulePath, err.Error())
				// continue scanning other modules
			}

			results[modulePath] = append(results[modulePath], module.Analysis{
				AnalyzerName: analyzer.Name,
				Graphs:       scanned,
			})
		}
	}

	return results
}
