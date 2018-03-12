package config

import (
	"fmt"
	"os"
	"strings"
	"time"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

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

// New creates a CLIConfig from cli context
func New(c *cli.Context) (CLIConfig, error) {
	modules, err := parseModulesFlag(c.String("modules"))
	if err != nil {
		return CLIConfig{}, err
	}

	var config = CLIConfig{
		APIKey:         c.String("api_key"),
		Fetcher:        c.String("fetcher"),
		Project:        c.String("project"),
		Revision:       c.String("revision"),
		Endpoint:       c.String("endpoint"),
		Modules:        modules,
		Debug:          c.Bool("debug"),
		ConfigFilePath: c.String("config"),

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
			Locators: c.Bool("locators"),
			Data:     c.String("data"),
		},

		ReportCmd: ReportConfig{
			Type: c.String("type"),
		},
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
	if config.Debug {
		formatter := logging.MustStringFormatter(`%{color}%{time} %{level} %{module}:%{shortpkg}/%{shortfile}/%{shortfunc}%{color:reset} %{message}`)
		stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
		stderrBackend.SetLevel(logging.DEBUG, "")
		logging.SetBackend(stderrBackend)
	} else {
		formatter := logging.MustStringFormatter(`%{color}%{level}%{color:reset} %{message}`)
		stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
		stderrBackend.SetLevel(logging.WARNING, "")
		logging.SetBackend(stderrBackend)
	}

	configLogger.Debugf("Configuration initialized: %#v", config)

	return config, nil
}
