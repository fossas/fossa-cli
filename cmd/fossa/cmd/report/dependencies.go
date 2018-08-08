package report

import (
	"fmt"

	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/urfave/cli"
)

var dependenciesCmd = cli.Command{
	Name:   "dependencies",
	Usage:  "Generate dependencies report",
	Flags:  flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions(flags.WithReportTemplateFlags([]cli.Flag{})))),
	Action: dependenciesRun,
}

func dependenciesRun(ctx *cli.Context) error {
	analyzed, err := analyzeModules(ctx)
	if err != nil {
		log.Logger.Fatal("Could not analyze modules: %s", err.Error())
	}

	pkgs := make([]pkg.Package, 0)
	for _, module := range analyzed {
		for _, pkg := range module.Deps {
			pkgs = append(pkgs, pkg)
		}
	}

	if tmplFile := ctx.String(flags.Template); tmplFile != "" {
		output, err := display.TemplateFile(tmplFile, pkgs)
		fmt.Println(output)
		if err != nil {
			log.Logger.Fatalf("Could not parse template data: %s", err.Error())
		}
		return nil
	}

	return log.PrintJSON(pkgs)
}
