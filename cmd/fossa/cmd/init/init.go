package init

import (
	"fmt"
	"os"
	"regexp"
	"time"

	"github.com/briandowns/spinner"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/builders"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

var Cmd = cli.Command{
	Name:   "init",
	Usage:  "Initialize a .fossa.yml configuration file",
	Action: Run,
	Flags: flags.WithGlobalFlags([]cli.Flag{
		cli.BoolFlag{Name: "O, overwrite", Usage: "overwrite modules in config even if they exist"},
		cli.BoolFlag{Name: "include-all", Usage: "include suspicious modules (`docs`, `test` or `example` in name)"},
	}),
}

func Run(ctx *cli.Context) {}

func Do() {}

func initCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		log.Logger.Fatalf("Could not load configuration: %s", err.Error())
	}

	if err := doInit(&conf, c.Bool("overwrite"), c.Bool("include-all")); err != nil {
		log.Logger.Fatalf("Error initializing: %s", err.Error())
	}

	if err := config.WriteConfigFile(&conf); err != nil {
		log.Logger.Fatalf("Error writing config: %s", err.Error())
	}

	log.Logger.Warningf("Config for %d modules written to `%s`.", len(conf.Modules), conf.ConfigFilePath)

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
			log.Logger.Warningf("Warning during autoconfiguration: %s", err.Error())
		}

		if !includeAll {
			// Filter suspicious modules
			var filteredModuleConfigs []module.Config
			for _, c := range conf.Modules {
				if matched, err := regexp.MatchString("(docs?/|test|example|vendor/|node_modules/|.srclib-cache/|spec/|Godeps/|.git/|bower_components/|third_party/)", c.Path); err != nil || matched != true {
					filteredModuleConfigs = append(filteredModuleConfigs, c)
				} else {
					log.Logger.Warningf("Filtering out suspicious module: %s (%s)", c.Name, c.Path)
				}
			}
			conf.Modules = filteredModuleConfigs
		}
	} else {
		log.Logger.Warningf("%d module(s) found in config file (`%s`); skipping initialization.", len(conf.Modules), conf.ConfigFilePath)
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
			log.Logger.Warningf("No builder available for module type: %s", t)
		}
		foundModules, err := builder.DiscoverModules(dir)
		if err != nil {
			lastError = err
		}
		moduleConfigs = append(moduleConfigs, foundModules...)
	}
	return moduleConfigs, lastError
}
