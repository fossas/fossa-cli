package main

import (
	"fmt"
	"os"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

type buildConfig struct {
	force bool
}

var buildLogger = logging.MustGetLogger("build")

func buildCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		buildLogger.Fatalf("Could not load configuration: %s", err.Error())
	}
	if len(config.modules) == 0 {
		buildLogger.Fatal("No modules specified.")
	}

	for _, moduleConfig := range config.modules {
		builder, module, err := resolveModuleConfig(moduleConfig)
		if err != nil {
			buildLogger.Fatalf("Failed to resolve modules: %s", err.Error())
		}

		err = builder.Initialize()
		if err != nil {
			buildLogger.Fatalf("Failed to initialize build: %s", err.Error())
		}

		isBuilt, err := builder.IsBuilt(module, config.analyzeConfig.allowUnresolved)
		if err != nil {
			buildLogger.Fatalf("Could not determine whether module %s is built: %s", module.Name, err.Error())
		}
		if isBuilt && !config.buildConfig.force {
			buildLogger.Fatalf("Module %s appears to already be built. Refusing to continue. Use `--force` to force a rebuild.", module.Name)
		}

		err = builder.Build(module, config.buildConfig.force)
		if err != nil {
			buildLogger.Fatalf("Build failed on module %s: %s", module.Name, err.Error())
		}
	}

	fmt.Fprintln(os.Stderr, "Build succeeded, ready to analyze!")
}
