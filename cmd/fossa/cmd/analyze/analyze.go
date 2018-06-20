package analyze

import (
	"os"
	"text/template"
	"encoding/json"
	"fmt"
	"io/ioutil"

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
	Template = "template"
)

var Cmd = cli.Command{
	Name:      "analyze",
	Usage:     "Analyze built dependencies",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithModulesFlags([]cli.Flag{
		cli.StringFlag{Name: flags.Short(Output), Usage: "send analysis to output file instead of uploading to FOSSA (default: -)"},
		cli.StringFlag{Name: flags.Short(Template), Usage: "process analysis via template prior to sending it to output"},
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

	analyzed, err := AnalyzeModules(modules)
	if err != nil {
		log.Logger.Fatalf("Could not analyze modules: %s", err.Error())
		return err
	}

	normalized, err := fossa.Normalize(analyzed)
	if err != nil {
		log.Logger.Fatalf("Could not normalize output: %s", err.Error())
		return err
	}

	if ctx.String(Output) != "" || ctx.String(Template) != "" {
		return outputAnalysis(ctx.String(Output), ctx.String(Template), normalized)
	}

	return uploadAnalysis(normalized)
}

func AnalyzeModules(modules []module.Module) (analyzed []module.Module, err error) {
	defer log.StopSpinner()
	for i, m := range modules {
		log.ShowSpinner(fmt.Sprintf("Analyzing module (%d/%d): %s", i+1, len(modules), m.Name))
		analyzer, err := analyzers.New(m.Type, m.Options)
		if err != nil {
			log.Logger.Warningf("Could not load analyzer: %s", err.Error())
		}
		result, err := analyzer.Analyze(m)
		if err != nil {
			log.Logger.Warningf("Could not analyze: %s", err.Error())
		}
		analyzed = append(analyzed, result)
	}
	log.StopSpinner()

	return analyzed, err
}

func outputAnalysis(outputFile, templateFile string, normalized []fossa.SourceUnit) (err error) {
	var (
		msg    []byte
		tmpl   *template.Template
	)

	if templateFile == "" {
		msg, err = json.Marshal(normalized)
		if err != nil {
			log.Logger.Fatalf("Could not marshal output: %s", err.Error())
			return err
		}
	} else {
		tmpl, err = template.ParseFiles(templateFile)
		if err != nil {
			log.Logger.Fatalf("Could not parse template data: %s", err.Error())
			return err
		}
		msg, err = cmdutil.ProcessTmpl(tmpl, normalized)
		if err != nil {
			log.Logger.Fatalf("Could not process template data: %s", err.Error())
			return err
		}
	}

	if outputFile == "-" {
		_, err = os.Stdout.Write(msg)
		return err
	}

	return ioutil.WriteFile(outputFile, msg, 0664)
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