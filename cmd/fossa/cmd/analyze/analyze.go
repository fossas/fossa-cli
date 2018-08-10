package analyze

import (
	"fmt"

	"github.com/urfave/cli"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

var ShowOutput = "output"

var Cmd = cli.Command{
	Name:      "analyze",
	Usage:     "Analyze built dependencies",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: "show-output, output, o", Usage: "print results to stdout instead of uploading to FOSSA"},
		flags.TemplateF,
	}))),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.Setup(ctx)
	if err != nil {
		log.Fatalf("Could not initialize: %s", err.Error())
	}

	modules, err := config.Modules()
	if err != nil {
		log.Fatalf("Could not parse modules: %s", err.Error())
	}
	if len(modules) == 0 {
		log.Fatal("No modules specified.")
	}

	analyzed, err := Do(modules)
	if err != nil {
		log.Fatalf("Could not analyze modules: %s", err.Error())
		return err
	}

	log.Logger.Debugf("analyzed: %#v", analyzed)
	normalized, err := fossa.Normalize(analyzed)
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
			display.JSON(normalized)
		}

		return nil
	}

	return uploadAnalysis(normalized)
}

func Do(modules []module.Module) (analyzed []module.Module, err error) {
	defer display.ClearProgress()
	for i, m := range modules {
		display.InProgress(fmt.Sprintf("Analyzing module (%d/%d): %s", i+1, len(modules), m.Name))

		// Handle raw modules differently from all others.
		// TODO: maybe this should occur during the analysis step?
		// TODO: maybe this should target a third-party folder, rather than a single
		// folder? Maybe "third-party folder" should be a separate module type?
		if m.Type == pkg.Raw {
			locator, err := fossa.UploadTarball(m.BuildTarget)
			if err != nil {
				log.Warnf("Could not upload raw module: %s", err.Error())
			}
			id := pkg.ID{
				Type:     pkg.Raw,
				Name:     locator.Project,
				Revision: locator.Revision,
			}
			m.Imports = []pkg.Import{pkg.Import{Resolved: id}}
			m.Deps = make(map[pkg.ID]pkg.Package)
			m.Deps[id] = pkg.Package{
				ID: id,
			}
			analyzed = append(analyzed, m)
			continue
		}

		analyzer, err := analyzers.New(m)
		if err != nil {
			analyzed = append(analyzed, m)
			log.Warnf("Could not load analyzer: %s", err.Error())
			continue
		}
		built, err := analyzer.IsBuilt()
		if err != nil {
			log.Warnf("Could not determine whether module is built: %s", err.Error())
		}
		if !built {
			log.Warnf("Module does not appear to be built")
		}
		deps, err := analyzer.Analyze()
		if err != nil {
			log.Fatalf("Could not analyze: %s", err.Error())
		}
		m.Imports = deps.Direct
		m.Deps = deps.Transitive
		analyzed = append(analyzed, m)
	}
	display.ClearProgress()

	return analyzed, err
}

func uploadAnalysis(normalized []fossa.SourceUnit) error {
	display.InProgress("Uploading analysis...")
	locator, err := fossa.Upload(config.Fetcher(), config.Project(), config.Revision(), config.Title(), config.Branch(), normalized)
	display.ClearProgress()
	if err != nil {
		log.Fatalf("Error during upload: %s", err.Error())
		return err
	}
	fmt.Printf(display.ReportURL(locator))
	return nil
}
