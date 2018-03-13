package main

import (
	"fmt"
	"os"
	"time"

	"github.com/briandowns/spinner"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
	"github.com/op/go-logging"
)

var buildLogger = logging.MustGetLogger("build")

func buildCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		buildLogger.Fatalf("Could not load configuration: %s", err.Error())
	}
	if len(conf.Modules) == 0 {
		buildLogger.Fatal("No modules specified.")
	}

	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Initializing..."
	s.Start()

	for i, moduleConfig := range conf.Modules {
		s.Suffix = fmt.Sprintf(" Running module build (%d/%d): %s", i+1, len(conf.Modules), moduleConfig.Name)
		s.Restart()
		err := doBuild(moduleConfig, conf.AnalyzeCmd.AllowUnresolved, conf.BuildCmd.Force)
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Could not run build: %s", err.Error())
		}
	}
	s.Stop()
	fmt.Fprintln(os.Stderr, "Build succeeded, ready to analyze!")
}

func doBuild(moduleConfig module.Config, allowUnresolved, force bool) error {
	builder, module, err := resolveModuleConfig(moduleConfig)
	if err != nil {
		return fmt.Errorf("failed to resolve modules: %s", err.Error())
	}

	err = builder.Initialize()
	if err != nil {
		return fmt.Errorf("failed to initialize build: %s", err.Error())
	}

	isBuilt, err := builder.IsBuilt(module, allowUnresolved)
	if err != nil {
		return fmt.Errorf("could not determine whether module %s is built: %s", module.Name, err.Error())
	}
	if isBuilt && !force {
		return fmt.Errorf("module %s appears to already be built (use `--force` to force a rebuild)", module.Name)
	}

	err = builder.Build(module, force)
	if err != nil {
		return fmt.Errorf("build failed on module %s: %s", module.Name, err.Error())
	}

	return nil
}
