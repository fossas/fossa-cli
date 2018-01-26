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
	Cli struct {
		APIKey  string `yaml:"api_key"`
		Server  string
		Project string
		Locator string
	}
	Analyze struct {
		Ignores []struct {
			Path string
			Type string
		}
		Modules []struct {
			Name string
			Path string
			Type string
		}
	}
}

// ReadConfig parses the configuration file in the current directory and sets
// default values if necessary.
func ReadConfig() (*Config, error) {
	_, err := os.Stat(".fossa.yml")
	if err == nil {
		return parseConfig(".fossa.yml")
	}

	_, err = os.Stat(".fossa.yaml")
	if err == nil {
		return parseConfig(".fossa.yaml")
	}

	return nil, nil
}

func parseConfig(filename string) (*Config, error) {
	var config Config

	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	err = yaml.Unmarshal(bytes, &config)
	if err != nil {
		return nil, err
	}

	if len(config.Cli.Server) == 0 {
		config.Cli.Server = "https://app.fossa.io"
	}

	if len(config.Cli.Locator) == 0 {
		repo, err := git.PlainOpen(".")
		if err != nil {
			return nil, err
		}

		project := config.Cli.Project
		if len(project) == 0 {
			origin, err := repo.Remote("origin")
			if err != nil {
				return nil, err
			}
			if origin == nil {
				return nil, errors.New("could not infer project name from either `.fossa.yaml` or `git` remote named `origin`")
			}
			project = origin.Config().URLs[0]
			config.Cli.Project = project
		}

		revision, err := repo.Head()
		if err != nil {
			return nil, err
		}
		config.Cli.Locator = project + "$" + revision.Hash().String()
	}

	return &config, nil
}
