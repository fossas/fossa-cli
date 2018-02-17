package main

import (
	"errors"
	"io/ioutil"
	"os"

	"github.com/fossas/fossa-cli/module"
	git "gopkg.in/src-d/go-git.v4"
	yaml "gopkg.in/yaml.v2"
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
			return configFileV1{}, errors.New("invalid config file specified")
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
