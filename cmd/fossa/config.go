package main

import (
	"errors"
	"io/ioutil"
	"os"

	git "gopkg.in/src-d/go-git.v4"
	yaml "gopkg.in/yaml.v2"
)

// Config is a parsed configuration for the CLI and Worker.
type Config struct {
	// config version
	Version string

	CLI struct {
		// Upload configuration.
		APIKey  string `yaml:"api_key"`
		Server  string
		Project string
		Locator string

		// CLI flags.
		Install  bool
		Upload   bool
		NoCache  bool
		LogLevel string
	}
	Analyze struct {
		Ignores []struct {
			Path string
			Type string
		}
		Modules []ModuleConfig
	}
}

// ModuleConfig is the configuration for a single module to be analysed.
type ModuleConfig struct {
	Name string
	Path string
	Type string
}

// ReadConfig parses the configuration file in the current directory and sets
// default values if necessary.
func ReadConfig(configFile string) (Config, error) {
	if configFile != "" {
		if _, err := os.Stat(configFile); err != nil {
			return Config{}, errors.New("invalid config file specified")
		}
		return parseConfig(configFile)
	}

	_, err := os.Stat(".fossa.yml")
	if err == nil {
		return parseConfig(".fossa.yml")
	}

	_, err = os.Stat(".fossa.yaml")
	if err == nil {
		return parseConfig(".fossa.yaml")
	}

	return setDefaultValues(Config{})
}

func parseConfig(filename string) (Config, error) {
	// Read configuration file.
	var config Config

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

func setDefaultValues(c Config) (Config, error) {
	// Set config version
	if c.Version == "" {
		c.Version = "1"
	}

	// Set default endpoint.
	if len(c.CLI.Server) == 0 {
		c.CLI.Server = "https://app.fossa.io"
	}

	// Load API key from environment variable.
	if len(c.CLI.APIKey) == 0 {
		c.CLI.APIKey = os.Getenv("FOSSA_API_KEY")
	}

	// Infer default locator and project from `git`.
	if len(c.CLI.Locator) == 0 {
		repo, err := git.PlainOpen(".")
		if err == nil {
			project := c.CLI.Project
			if len(project) == 0 {
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
