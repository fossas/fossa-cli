package config

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/mattn/go-isatty"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

func parseModulesFlag(moduleFlag string) ([]module.Config, error) {
	if moduleFlag == "" {
		return []module.Config{}, nil
	}
	var config []module.Config

	modules := strings.Split(moduleFlag, ",")
	for _, m := range modules {
		sections := strings.Split(m, ":")
		if len(sections) != 2 {
			return nil, fmt.Errorf("invalid modules flag: %s", moduleFlag)
		}
		config = append(config, module.Config{
			Name: sections[1],
			Path: sections[1],
			Type: sections[0],
		})
	}

	return config, nil
}

func TryBool(c *cli.Context, flag string) bool {
	return c.Bool(flag) || c.GlobalBool(flag)
}

// New creates a CLIConfig from a *cli.Context
func New(c *cli.Context) (CLIConfig, error) {
	// TODO: the `strings.Join` is a compatibility hack for now
	modules, err := parseModulesFlag(strings.Join(c.StringSlice("modules"), ","))
	if err != nil {
		return CLIConfig{}, err
	}

	var config = CLIConfig{
		APIKey:   c.String("api_key"),
		Fetcher:  c.String("fetcher"),
		Project:  c.String("project"),
		Revision: c.String("revision"),
		Branch:   c.String("branch"),
		Endpoint: c.String("endpoint"),
		Modules:  modules,

		Debug:       TryBool(c, "debug"),
		Interactive: isatty.IsTerminal(os.Stdout.Fd()) && !TryBool(c, "no-ansi"),

		DefaultCmd: DefaultConfig{
			Build: c.Bool("build"),
		},

		AnalyzeCmd: AnalyzeConfig{
			Output:          c.Bool("output"),
			AllowUnresolved: c.Bool("allow-unresolved"),
		},

		BuildCmd: BuildConfig{
			Force: c.Bool("force"),
		},

		TestCmd: TestConfig{
			Timeout: time.Duration(c.Int("timeout")) * time.Second,
		},

		UploadCmd: UploadConfig{
			UseLocators: c.Bool("locators"),
			Data:        c.String("data"),
		},

		ReportCmd: ReportConfig{
			Type: c.String("type"),
		},

		ConfigFilePath: c.String("config"),
		Version:        c.App.Metadata["version"].(string),
	}

	// Load configuration file and set overrides.
	configFilePath, configFile, err := readConfigFile(config.ConfigFilePath)
	config.ConfigFilePath = configFilePath
	if err != nil {
		return CLIConfig{}, err
	}

	if config.Project == "" {
		config.Project = configFile.CLI.Project
	}
	if config.Revision == "" {
		config.Revision = configFile.CLI.Revision
	}

	if config.Fetcher == "" {
		config.Fetcher = configFile.CLI.Fetcher
	}

	if config.APIKey == "" {
		config.APIKey = configFile.CLI.APIKey
	}
	if config.Endpoint == "" {
		config.Endpoint = configFile.CLI.Server
	}
	if len(config.Modules) == 0 {
		config.Modules = configFile.Analyze.Modules
	}

	// Configure logging.
	log.Initialize(config.Interactive, config.Debug)

	log.Logger.Debugf("Configuration initialized: %#v", config)
	return config, nil
}

// MustNew calls New but fails on an error instead of returning the error
func MustNew(c *cli.Context) CLIConfig {
	config, err := New(c)
	if err != nil {
		log.Logger.Fatalf("Could not initialize configuration: %s", err.Error())
	}
	return config
}
