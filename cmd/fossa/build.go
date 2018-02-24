package main

import (
	"fmt"
	"os"

	"github.com/urfave/cli"

	config "github.com/fossas/fossa-cli/config"
	logging "github.com/op/go-logging"
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

	for _, moduleConfig := range conf.Modules {
		builder, module, err := resolveModuleConfig(moduleConfig)
		if err != nil {
			buildLogger.Fatalf("Failed to resolve modules: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			buildLogger.Fatalf("Failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module, conf.AnalyzeCmd.AllowUnresolved)
		if err != nil {
			buildLogger.Fatalf("Could not determine whether module %s is built: %s", module.Name, err.Error())
		}
		if isBuilt && !conf.BuildCmd.Force {
			buildLogger.Fatalf("Module %s appears to already be built. Refusing to continue. Use `--force` to force a rebuild.", module.Name)
		}

		err = builder.Build(module, conf.BuildCmd.Force)
		if err != nil {
			buildLogger.Fatalf("Build failed on module %s: %s", module.Name, err.Error())
		}
	}

	fmt.Fprintln(os.Stderr, "Build succeeded, ready to analyze!")
}
