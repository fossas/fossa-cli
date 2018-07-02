package report

import (
	"text/template"

	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/urfave/cli"
)

var dependenciesCmd = cli.Command{
	Name:   "dependencies",
	Usage:  "Generate dependencies report",
	Flags:  flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithModulesFlags(flags.WithReportTemplateFlags([]cli.Flag{})))),
	Before: prepareReportCtx,
	Action: generateDependencies,
}

func generateDependencies(ctx *cli.Context) (err error) {
	pkgs := make([]pkg.Package, 0)
	for _, module := range analyzed {
		for _, pkg := range module.Deps {
			pkgs = append(pkgs, pkg)
		}
	}

	var tmpl *template.Template

	if ctx.String(flags.Template) != "" {
		tmpl, err = template.ParseFiles(ctx.String(flags.Template))
		if err != nil {
			log.Logger.Fatalf("Could not parse template data: %s", err.Error())
		}
	}

	return cmdutil.OutputData(ctx.String(flags.ShowOutput), tmpl, pkgs)
}
