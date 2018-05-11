package main

import (
	"fmt"
	"os"
	"regexp"
	"time"

	"github.com/briandowns/spinner"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/builders"
	config "github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

func initCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		log.Fatalf("Could not load configuration: %s", err.Error())
	}

	if err := doInit(&conf, c.Bool("overwrite"), c.Bool("include-all")); err != nil {
		log.Fatalf("Error initializing: %s", err.Error())
	}

	if err := config.WriteConfigFile(&conf); err != nil {
		log.Fatalf("Error writing config: %s", err.Error())
	}

	log.Warningf("Config for %d modules written to `%s`.", len(conf.Modules), conf.ConfigFilePath)

	fmt.Println("`fossa` is initialized")
}

func doInit(conf *config.CLIConfig, overwrite bool, includeAll bool) error {
	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Initializing..."
	s.Start()
	findDir := "."
	if len(conf.Modules) == 0 || overwrite {
		if cwd, err := os.Getwd(); err == nil {
			findDir = cwd
		}
		var err error
		conf.Modules, err = findModules(findDir)
		if err != nil {
			log.Warningf("Warning during autoconfiguration: %s", err.Error())
		}

		if !includeAll {
			// Filter suspicious modules
			var filteredModuleConfigs []module.Config
			for _, c := range conf.Modules {
				if matched, err := regexp.MatchString("(docs?/|test|example|vendor/|node_modules/|.srclib-cache/|spec/|Godeps/|.git/|bower_components/|third_party/)", c.Path); err != nil || matched != true {
					filteredModuleConfigs = append(filteredModuleConfigs, c)
				} else {
					log.Warningf("Filtering out suspicious module: %s (%s)", c.Name, c.Path)
				}
			}
			conf.Modules = filteredModuleConfigs
		}
	} else {
		log.Warningf("%d module(s) found in config file (`%s`); skipping initialization.", len(conf.Modules), conf.ConfigFilePath)
	}
	s.Stop()
	return nil
}

// `findModules` calls DiscoverModules() on all available integrations and returns a new config state
// If this function errors, this is not necessarily fatal; it just means one of the scanners failed
func findModules(dir string) ([]module.Config, error) {
	var lastError error
	var moduleConfigs []module.Config
	for _, t := range module.Types {
		builder := builders.New(t)
		if builder == nil {
			log.Warningf("No builder available for module type: %s", t)
		}
		foundModules, err := builder.DiscoverModules(dir)
		if err != nil {
			lastError = err
		}
		moduleConfigs = append(moduleConfigs, foundModules...)
	}
	return moduleConfigs, lastError
}
