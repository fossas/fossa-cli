package report

import (
	"text/template"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/urfave/cli"
)

var dependenciesCmd = cli.Command{
	Name:      "dependencies",
	Usage:     "Generate dependencies report",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithModulesFlags([]cli.Flag{
		cli.StringFlag{Name: flags.Short(Output), Destination: &outputFlag, Value: "-", Usage: "Output file for report"},
		cli.StringFlag{Name: flags.Short(Template), Destination: &templateFlag, Usage: "process report via template prior to sending it to output"},
	}))),
	Before: prepareReportCtx,
	Action: generateDependencies,
}

func generateDependencies(ctx *cli.Context) (err error) {
	pkgs := make([]pkg.Package, 0)
	for _,  module := range analyzed {
		for _, pkg := range module.Deps {
			pkgs = append(pkgs, pkg)
		}
	}

	var tmpl *template.Template

	if templateFlag != "" {
		tmpl, err = template.ParseFiles(templateFlag)
		if err != nil {
			log.Logger.Fatalf("Could not parse template data: %s", err.Error())
		}
	}

	return outputReport(outputFlag, tmpl, pkgs)
}