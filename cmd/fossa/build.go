package main

import (
	"fmt"
	"os"
	"time"

	"github.com/briandowns/spinner"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
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
		builder, module, err := resolveModuleConfig(moduleConfig)
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Failed to resolve modules: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module, conf.AnalyzeCmd.AllowUnresolved)
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Could not determine whether module %s is built: %s", module.Name, err.Error())
		}
		if isBuilt && !conf.BuildCmd.Force {
			s.Stop()
			buildLogger.Fatalf("Module %s appears to already be built. Refusing to continue. Use `--force` to force a rebuild.", module.Name)
		}

		err = builder.Build(module, conf.BuildCmd.Force)
		if err != nil {
			s.Stop()
			buildLogger.Fatalf("Build failed on module %s: %s", module.Name, err.Error())
		}
	}
	s.Stop()
	fmt.Fprintln(os.Stderr, "Build succeeded, ready to analyze!")
}
