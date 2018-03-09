package config

import (
	"fmt"
	"os"
	"strings"
	"time"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var configLogger = logging.MustGetLogger("config")

// DefaultConfig specifies the config for the default command
type DefaultConfig struct {
	Build bool
}

// AnalyzeConfig specifies the config for the analyze command
type AnalyzeConfig struct {
	Output          bool
	AllowUnresolved bool
}

// BuildConfig specifies the config for the build command
type BuildConfig struct {
	Force bool
}

// TestConfig specifies the config for the test command
type TestConfig struct {
	Timeout time.Duration
}

// UploadConfig specifies the config for the upload command
type UploadConfig struct {
	Locators bool
	Data     string
}

// ReportConfig specifies the config for the report command
type ReportConfig struct {
	Type string // Either "dependencies" or "licenses"
}

// CLIConfig specifies the config available to the cli
type CLIConfig struct {
	APIKey       string
	Fetcher      string
	Project      string
	Revision     string
	Endpoint     string
	Modules      []ModuleConfig
	Debug        bool
	LoggingLevel logging.Level

	DefaultCmd DefaultConfig
	AnalyzeCmd AnalyzeConfig
	BuildCmd   BuildConfig
	TestCmd    TestConfig
	UploadCmd  UploadConfig
	ReportCmd  ReportConfig

	ConfigFilePath string
}

// MakeLocator creates a locator string given a package and revision
func MakeLocator(fetcher string, project string, revision string) string {
	if fetcher != "git" {
		return fetcher + "+" + project + "$" + revision
	}

	return "git+" + normalizePackageSpec(project) + "$" + revision
}

func normalizePackageSpec(project string) string {
	// Remove fetcher prefix (in case project is derived from splitting a locator on '$')
	noFetcherPrefix := strings.TrimPrefix(project, "git+")

	// Normalise Git URL format
	noGitExtension := strings.TrimSuffix(noFetcherPrefix, ".git")
	handleGitHubSSH := strings.Replace(noGitExtension, "git@github.com:", "github.com/", 1)

	// Remove protocols
	noHTTPPrefix := strings.TrimPrefix(handleGitHubSSH, "http://")
	noHTTPSPrefix := strings.TrimPrefix(noHTTPPrefix, "https://")

	return noHTTPSPrefix
}

func parseModulesFlag(moduleFlag string) ([]ModuleConfig, error) {
	if moduleFlag == "" {
		return []ModuleConfig{}, nil
	}
	var config []ModuleConfig

	modules := strings.Split(moduleFlag, ",")
	for _, m := range modules {
		sections := strings.Split(m, ":")
		if len(sections) != 2 {
			return nil, fmt.Errorf("invalid modules flag: %s", moduleFlag)
		}
		config = append(config, ModuleConfig{
			Name: sections[1],
			Path: sections[1],
			Type: sections[0],
		})
	}

	return config, nil
}

func configureLoggingLevel(logFlag string) (logging.Level, error) {
	// We default to a log level of WARNING
	if logFlag == "" {
		return logging.WARNING, nil
	}

	// NOTE: we currently only use these flags in the code base (3/7/18)
	switch logFlag {
	case "error":
		return logging.ERROR, nil
	case "warning":
		return logging.WARNING, nil
	case "notice":
		return logging.NOTICE, nil
	case "info":
		return logging.INFO, nil
	case "debug":
		return logging.DEBUG, nil
	}

	return -1, fmt.Errorf("unknown logging level: %s", logFlag)
}

// New creates a CLIConfig from cli context
func New(c *cli.Context) (CLIConfig, error) {
	modules, err := parseModulesFlag(c.String("modules"))
	if err != nil {
		return CLIConfig{}, err
	}

	// defaults to a log level of WARNING
	logLevel, err := configureLoggingLevel(c.String("log-level"))
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
		LoggingLevel:   logLevel,

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
		stderrBackend.SetLevel(config.LoggingLevel, "") // configured above
		logging.SetBackend(stderrBackend)
	}

	configLogger.Debugf("Configuration initialized: %#v", config)

	return config, nil
}
