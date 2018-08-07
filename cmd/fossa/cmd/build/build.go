package build

import (
	"fmt"

	"github.com/pkg/errors"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/analyzers"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

var (
	Clean = "clean"
	Force = "force"
)

var Cmd = cli.Command{
	Name:      "build",
	Usage:     "Run a default project build",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithOptions([]cli.Flag{
		cli.BoolFlag{Name: Clean, Usage: "clean artifacts before building"},
		cli.BoolFlag{Name: Force, Usage: "rebuild module even if it appears to already be built"},
	})),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.Setup(ctx)
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

	defer log.StopSpinner()
	for i, m := range modules {
		log.ShowSpinner(fmt.Sprintf("Building module (%d/%d): %s", i+1, len(modules), m.Name))
		err := Do(m, ctx.Bool(Clean), ctx.Bool(Force))
		if err != nil {
			log.Logger.Warningf("Could not build: %s", err.Error())
		}
	}
	log.Logger.Notice("Build succeeded, ready to analyze!")
	return nil
}

func Do(m module.Module, clean, force bool) error {
	analyzer, err := analyzers.New(m)
	if err != nil {
		return err
	}
	if clean {
		err := analyzer.Clean()
		if err != nil {
			return errors.Wrap(err, "could not clean module build")
		}
	}
	built, err := analyzer.IsBuilt()
	if err != nil {
		return errors.Wrap(err, "could not determine whether module is built")
	}
	if built {
		if force {
			log.Logger.Warningf("Module %s appears to be built, but --force was passed: building anyway...", m.Name)
		} else {
			return errors.New("module appears to already be built")
		}
	}
	err = analyzer.Build()
	if err != nil {
		return err
	}
	return nil
}
