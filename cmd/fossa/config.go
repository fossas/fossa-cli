package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
	git "gopkg.in/src-d/go-git.v4"
	yaml "gopkg.in/yaml.v2"

	"github.com/fossas/fossa-cli/module"
)

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
		Modules []moduleConfig
	}
}

type moduleConfig struct {
	Name string
	Path string
	Type module.Type
}

func readConfigFile(path string) (configFileV1, error) {
	if path != "" {
		if _, err := os.Stat(path); err != nil {
			return configFileV1{}, fmt.Errorf("invalid config file specified")
		}
		return parseConfigFile(path)
	}

	_, err := os.Stat(".fossa.yml")
	if err == nil {
		return parseConfigFile(".fossa.yml")
	}

	_, err = os.Stat(".fossa.yaml")
	if err == nil {
		return parseConfigFile(".fossa.yaml")
	}

	return setDefaultValues(configFileV1{})
}

func parseConfigFile(filename string) (configFileV1, error) {
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

type cliConfig struct {
	apiKey   string
	project  string
	revision string
	endpoint string
	modules  []moduleConfig
	debug    bool

	defaultConfig defaultConfig
	analyzeConfig analyzeConfig
	buildConfig   buildConfig
	testConfig    testConfig
	uploadConfig  uploadConfig
}

func makeLocator(project string, revision string) string {
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

type defaultConfig struct {
	build bool
}

func parseModuleFlag(moduleFlag string) []moduleConfig {
	if moduleFlag == "" {
		return []moduleConfig{}
	}
	var config []moduleConfig

	modules := strings.Split(moduleFlag, ",")
	for _, m := range modules {
		sections := strings.Split(m, ":")
		config = append(config, moduleConfig{
			Name: sections[1],
			Path: sections[1],
			Type: module.Type(sections[0]),
		})
	}

	return config
}

func initialize(c *cli.Context) (cliConfig, error) {
	var config = cliConfig{
		apiKey:   c.String("api_key"),
		project:  c.String("project"),
		revision: c.String("revision"),
		endpoint: c.String("endpoint"),
		modules:  parseModuleFlag(c.String("modules")),
		debug:    c.Bool("debug"),

		defaultConfig: defaultConfig{
			build: c.Bool("build"),
		},

		analyzeConfig: analyzeConfig{
			output:          c.Bool("output"),
			allowUnresolved: c.Bool("allow-unresolved"),
			noUpload:        c.Bool("no-upload"),
		},

		buildConfig: buildConfig{
			force: c.Bool("force"),
		},

		testConfig: testConfig{
			timeout: time.Duration(c.Int("timeout")) * time.Second,
		},

		uploadConfig: uploadConfig{
			data: c.String("data"),
		},
	}

	// Load configuration file and set overrides.
	configFile, err := readConfigFile(c.String("config"))
	if err != nil {
		return cliConfig{}, err
	}

	var locatorSections []string
	var locatorProject string
	var locatorRevision string

	if configFile.CLI.Locator != "" {
		locatorSections = strings.Split(configFile.CLI.Locator, "$")
		locatorProject = strings.TrimPrefix(locatorSections[0], "git+")
		locatorRevision = locatorSections[1]
	}
	if config.project == "" {
		config.project = configFile.CLI.Project
		if config.project == "" {
			config.project = locatorProject
		}
	}
	if config.revision == "" {
		config.revision = locatorRevision
	}

	if config.apiKey == "" {
		config.apiKey = configFile.CLI.APIKey
	}
	if config.endpoint == "" {
		config.endpoint = configFile.CLI.Server
	}
	if len(config.modules) == 0 {
		config.modules = configFile.Analyze.Modules
	}

	// Configure logging.
	if config.debug {
		formatter := logging.MustStringFormatter(`%{color}%{time} %{level} %{module}:%{shortpkg}/%{shortfile}/%{shortfunc}%{color:reset} %{message}`)
		stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
		stderrBackend.SetLevel(logging.DEBUG, "")
		logging.SetBackend(stderrBackend)
	} else {
		formatter := logging.MustStringFormatter(`%{color}%{level}%{color:reset} %{message}`)
		stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), formatter))
		stderrBackend.SetLevel(logging.INFO, "")
		logging.SetBackend(stderrBackend)
	}

	mainLogger.Debugf("Configuration initialized: %#v", config)

	return config, nil
}
