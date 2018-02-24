package main

import (
	"fmt"
	"regexp"

	"github.com/fossas/fossa-cli/builders"
	config "github.com/fossas/fossa-cli/config"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var initLogger = logging.MustGetLogger("init")

func initCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		initLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	if err := doInit(&conf, c.Bool("force"), c.Bool("include-all")); err != nil {
		initLogger.Fatalf("Error initializing: %s", err.Error())
	}

	if !c.Bool("skip-write") {
		if err := config.WriteConfigFile(&conf); err != nil {
			initLogger.Fatalf("Error writing config: %s", err.Error())
		}

		initLogger.Warningf("Config for %d modules written to `%s`.", len(conf.Modules), conf.ConfigFilePath)
	}

	fmt.Println("`fossa` is initialized")
}

func doInit(conf *config.CliConfig, force bool, includeAll bool) error {
	var err error
	if len(conf.Modules) == 0 || force {
		conf.Modules, err = findModules(".")
		if err != nil {
			initLogger.Warningf("A non-fatal error occured during module discovery: %s", err.Error())
		}

		if !includeAll {
			// fitler suspicious modules
			filteredModuleConfigs := []config.ModuleConfig{}
			for _, c := range conf.Modules {
				if matched, err := regexp.MatchString("(docs?/|test|example)", c.Path); err != nil || matched != true {
					filteredModuleConfigs = append(filteredModuleConfigs, c)
				} else {
					initLogger.Warningf("Filtering out suspcious module: %s (%s)", c.Name, c.Path)
				}
			}
			conf.Modules = filteredModuleConfigs
		}
	} else {
		initLogger.Warningf("%d module(s) found in config file (`%s`); skipping initialization.", len(conf.Modules), conf.ConfigFilePath)
	}
	return nil
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
