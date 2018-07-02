package report

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"text/template"

	"github.com/fossas/fossa-cli/cmd/fossa/cmd/analyze"
	"github.com/fossas/fossa-cli/cmd/fossa/cmdutil"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/urfave/cli"
)

var (
	Unknown  = "show-unknown"
	analyzed []module.Module
)

var Cmd = cli.Command{
	Name:  "report",
	Usage: "Generate reports",
	Subcommands: []cli.Command{
		dependenciesCmd,
		licensesCmd,
	},
}

func prepareReportCtx(ctx *cli.Context) (err error) {
	err = cmdutil.InitWithAPI(ctx)
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

	analyzed, err = analyze.Modules(modules)
	if err != nil {
		log.Logger.Fatalf("Could not analyze modules: %s", err.Error())
	}

	return nil
}

func outputReport(outputFile string, tmpl *template.Template, data interface{}) (err error) {
	var (
		msg []byte
	)

	if tmpl == nil {
		msg, err = json.Marshal(data)
		println()
		if err != nil {
			log.Logger.Fatalf("Could not marshal output: %s", err.Error())
			return err
		}
	} else {
		msg, err = cmdutil.ProcessTmpl(tmpl, data)
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
