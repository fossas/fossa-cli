package main

import (
	"github.com/fossas/fossa-cli/builders"
	config "github.com/fossas/fossa-cli/config"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var initLogger = logging.MustGetLogger("init")

func initCmd(c *cli.Context) {
	var err error
	var conf config.CliConfig

	conf, err = config.Initialize(c)
	if err != nil {
		initLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	conf.Modules, err = findModules(".")
	if err != nil {
		initLogger.Warningf("A non-fatal error occured during module discovery: %s", err.Error())
	}

	initLogger.Warningf("%v", conf)
}

// findModules calls DiscoverModules() on all available integrations and returns a new config state
// to implement fail-open behavior, do not handle the err returned by this function
func findModules(dir string) ([]config.ModuleConfig, error) {
	var lastError error
	moduleConfigs := []config.ModuleConfig{}
	for _, t := range config.ModuleTypes {
		builder := builders.New(t)
		foundModules, err := builder.DiscoverModules(dir)
		if err != nil {
			lastError = err
		}
		moduleConfigs = append(moduleConfigs, foundModules...)
	}
	return moduleConfigs, lastError
}
