package analyze

import (
	"fmt"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
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
	err := cmdutil.InitWithAPI(ctx)
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

	analyzed, err := Do(modules)
	if err != nil {
		log.Logger.Fatalf("Could not analyze modules: %s", err.Error())
		return err
	}

	log.Logger.Debugf("analyzed: %#v", analyzed)
	normalized, err := fossa.Normalize(analyzed)
	if err != nil {
		log.Logger.Fatalf("Could not normalize output: %s", err.Error())
		return err
	}

	if ctx.Bool(ShowOutput) {
		if tmplFile := ctx.String(flags.Template); tmplFile != "" {
			err := cmdutil.OutputWithTemplateFile(tmplFile, normalized)
			if err != nil {
				log.Logger.Fatalf("Could not parse template data: %s", err.Error())
			}
		} else {
			log.PrintJSON(normalized)
		}

		return nil
	}

	return uploadAnalysis(normalized)
}

func Do(modules []module.Module) (analyzed []module.Module, err error) {
	defer log.StopSpinner()
	for i, m := range modules {
		log.ShowSpinner(fmt.Sprintf("Analyzing module (%d/%d): %s", i+1, len(modules), m.Name))

		// Handle raw modules differently from all others.
		// TODO: maybe this should occur during the analysis step?
		// TODO: maybe this should target a third-party folder, rather than a single
		// folder? Maybe "third-party folder" should be a separate module type?
		if m.Type == pkg.Raw {
			locator, err := fossa.UploadTarball(m.BuildTarget)
			if err != nil {
				log.Logger.Warningf("Could not upload raw module: %s", err.Error())
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
			log.Logger.Warningf("Could not load analyzer: %s", err.Error())
			continue
		}
		built, err := analyzer.IsBuilt()
		if err != nil {
			log.Logger.Warningf("Could not determine whether module is built: %s", err.Error())
		}
		if !built {
			log.Logger.Warningf("Module does not appear to be built")
		}
		deps, err := analyzer.Analyze()
		if err != nil {
			log.Logger.Fatalf("Could not analyze: %s", err.Error())
		}
		m.Imports = deps.Direct
		m.Deps = deps.Transitive
		analyzed = append(analyzed, m)
	}
	log.StopSpinner()

	return analyzed, err
}

func uploadAnalysis(normalized []fossa.SourceUnit) error {
	fossa.MustInit(config.Endpoint(), config.APIKey())
	log.ShowSpinner("Uploading analysis...")
	locator, err := fossa.Upload(config.Fetcher(), config.Project(), config.Revision(), config.Title(), config.Branch(), normalized)
	log.StopSpinner()
	if err != nil {
		log.Logger.Fatalf("Error during upload: %s", err.Error())
		return err
	}
	log.Printf(cmdutil.FmtReportURL(locator))
	return nil
}
