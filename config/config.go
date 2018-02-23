package config

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
	git "gopkg.in/src-d/go-git.v4"
	yaml "gopkg.in/yaml.v2"
)

var configLogger = logging.MustGetLogger("config")

type configFileV1 struct {
	// config version
	Version string

	CLI struct {
		// Upload configuration.
		APIKey  string `yaml:"api_key"`
		Server  string
		Project string
		Locator string
	}
	Analyze struct {
		Modules []ModuleConfig
	}
}

// ModuleType is an enumeration of supported build system types
type ModuleType string

const (
	// Individual tools

	// Bower is the module type for bower.io
	Bower = ModuleType("bower")
	// Composer is the module type for getcomposer.org
	Composer = ModuleType("composer")
	// Maven is the module type for maven.apache.org
	Maven = ModuleType("maven")
	// SBT is the module type for scala-sbt.org
	SBT = ModuleType("sbt")

	// Ecosystems where many tools behave similarly

	// Ruby is the module type for Bundler (bundler.io)
	Ruby = ModuleType("ruby")
	// Nodejs is the module type for NPM (npmjs.org) and Yarn (yarnpkg.com)
	Nodejs = ModuleType("nodejs")
	// Golang is the module type for dep, glide, godep, govendor, vndr, and manual
	// gopath vendoring
	Golang = ModuleType("golang")

	// VendoredArchives is a module type for archive formats (.tar, .rpm, .zip, etc...)
	VendoredArchives = ModuleType("vendoredarchives")
)

// ModuleConfig defines a config for a builder's entry point
type ModuleConfig struct {
	Name string
	Path string
	Type ModuleType
}

func readConfigFile(path string) (configFileV1, error) {
	if path != "" {
		if _, err := os.Stat(path); err != nil {
			return configFileV1{}, fmt.Errorf("invalid config file specified")
		}
		return ParseConfigFile(path)
	}

	_, err := os.Stat(".fossa.yml")
	if err == nil {
		return ParseConfigFile(".fossa.yml")
	}

	_, err = os.Stat(".fossa.yaml")
	if err == nil {
		return ParseConfigFile(".fossa.yaml")
	}

	return setDefaultValues(configFileV1{})
}

// WriteConfigFile writes a config state to yaml
func WriteConfigFile(filename string, conf configFileV1) error {
	return errors.New("Not Implemented")
}

// ParseConfigFile parses a file and returns a config state
func ParseConfigFile(filename string) (configFileV1, error) {
	// Read configuration file.
	var config configFileV1

	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return config, err
	}

	err = yaml.Unmarshal(bytes, &config)
	if err != nil {
		return config, err
	}

	config, err = setDefaultValues(config)
	if err != nil {
		return config, err
	}

	return config, nil
}

// SerializeConfigFile turns a config state to yaml
func SerializeConfigFile(configFileV1) (string, error) {
	return "", errors.New("Not Implemented")
}

func setDefaultValues(c configFileV1) (configFileV1, error) {
	// Set config version
	if c.Version == "" {
		c.Version = "1"
	}

	// Set default endpoint.
	if c.CLI.Server == "" {
		c.CLI.Server = os.Getenv("FOSSA_ENDPOINT")
		if c.CLI.Server == "" {
			c.CLI.Server = "https://app.fossa.io"
		}
	}

	// Load API key from environment variable.
	if c.CLI.APIKey == "" {
		c.CLI.APIKey = os.Getenv("FOSSA_API_KEY")
	}

	// Infer default locator and project from `git`.
	if c.CLI.Locator == "" {
		// TODO: this needs to happen in the module directory, not the working
		// directory
		repo, err := git.PlainOpen(".")
		if err == nil {
			project := c.CLI.Project
			if project == "" {
				origin, err := repo.Remote("origin")
				if err == nil && origin != nil {
					project = origin.Config().URLs[0]
					c.CLI.Project = project
				}
			}

			revision, err := repo.Head()
			if err == nil {
				c.CLI.Locator = "git+" + project + "$" + revision.Hash().String()
			}
		}
	}

	return c, nil
}

// AnalyzeConfig specifies the config for the analyze cmd
type AnalyzeConfig struct {
	Output          bool
	AllowUnresolved bool
	NoUpload        bool
}

// BuildConfig specifies the config for the build cmd
type BuildConfig struct {
	Force bool
}

// TestConfig specifies the config for the test cmd
type TestConfig struct {
	Timeout time.Duration
}

// UploadConfig specifies the config for the upload cmd
type UploadConfig struct {
	Data string
}

// CliConfig specifies the config available to the cli
type CliConfig struct {
	APIKey   string
	Project  string
	Revision string
	Endpoint string
	Modules  []ModuleConfig
	Debug    bool

	DefaultCmd DefaultConfig
	AnalyzeCmd AnalyzeConfig
	BuildCmd   BuildConfig
	TestCmd    TestConfig
	UploadCmd  UploadConfig
}

// MakeLocator creates a locator string given a package and revision
func MakeLocator(project string, revision string) string {
	// Remove fetcher prefix (in case project is derived from splitting a locator on '$')
	noFetcherPrefix := strings.TrimPrefix(project, "git+")

	// Normalise Git URL format
	noGitExtension := strings.TrimSuffix(noFetcherPrefix, ".git")
	handleGitHubSSH := strings.Replace(noGitExtension, "git@github.com:", "github.com/", 1)

	// Remove protocols
	noHTTPPrefix := strings.TrimPrefix(handleGitHubSSH, "http://")
	noHTTPSPrefix := strings.TrimPrefix(noHTTPPrefix, "https://")

	return "git+" + noHTTPSPrefix + "$" + revision
}

// DefaultConfig specifies the config for the default cmd
type DefaultConfig struct {
	Build bool
}

func parseModuleFlag(moduleFlag string) []ModuleConfig {
	if moduleFlag == "" {
		return []ModuleConfig{}
	}
	var config []ModuleConfig

	modules := strings.Split(moduleFlag, ",")
	for _, m := range modules {
		sections := strings.Split(m, ":")
		config = append(config, ModuleConfig{
			Name: sections[1],
			Path: sections[1],
			Type: ModuleType(sections[0]),
		})
	}

	return config
}

// Initialize creates a CliConfig from cli context
func Initialize(c *cli.Context) (CliConfig, error) {
	var config = CliConfig{
		APIKey:   c.String("api_key"),
		Project:  c.String("project"),
		Revision: c.String("revision"),
		Endpoint: c.String("endpoint"),
		Modules:  parseModuleFlag(c.String("modules")),
		Debug:    c.Bool("debug"),

		DefaultCmd: DefaultConfig{
			Build: c.Bool("build"),
		},

		AnalyzeCmd: AnalyzeConfig{
			Output:          c.Bool("output"),
			AllowUnresolved: c.Bool("allow-unresolved"),
			NoUpload:        c.Bool("no-upload"),
		},

		BuildCmd: BuildConfig{
			Force: c.Bool("force"),
		},

		TestCmd: TestConfig{
			Timeout: time.Duration(c.Int("timeout")) * time.Second,
		},

		UploadCmd: UploadConfig{
			Data: c.String("data"),
		},
	}

	// Load configuration file and set overrides.
	configFile, err := readConfigFile(c.String("config"))
	if err != nil {
		return CliConfig{}, err
	}

	var locatorSections []string
	var locatorProject string
	var locatorRevision string

	if configFile.CLI.Locator != "" {
		locatorSections = strings.Split(configFile.CLI.Locator, "$")
		locatorProject = strings.TrimPrefix(locatorSections[0], "git+")
		locatorRevision = locatorSections[1]
	}
	if config.Project == "" {
		config.Project = configFile.CLI.Project
		if config.Project == "" {
			config.Project = locatorProject
		}
	}
	if config.Revision == "" {
		config.Revision = locatorRevision
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
