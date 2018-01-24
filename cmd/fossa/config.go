package main

import (
	"io/ioutil"
	"os"

	yaml "gopkg.in/yaml.v2"
)

type Config struct {
	Cli struct {
		APIKey string `yaml:"api_key"`
		Server string
	}
	Analyze []struct {
		Name string
		Path string
		Type string
	}
	Build struct {
		WorkingDirectory string `yaml:"working_directory"`
		Docker           []struct {
			Image string
			Auth  struct {
				Username string
				Password string
			}
		}
		Environment map[string]string
		Steps       string
	}
}

func ReadConfig() (*Config, error) {
	_, err := os.Stat(".fossa.yml")
	if err == nil {
		return unmarshalConfig(".fossa.yml")
	}

	_, err = os.Stat(".fossa.yaml")
	if err == nil {
		return unmarshalConfig(".fossa.yaml")
	}

	return nil, nil
}

func unmarshalConfig(filename string) (*Config, error) {
	var config Config

	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	err = yaml.Unmarshal(bytes, &config)
	if err != nil {
		return nil, err
	}

	return &config, nil
}
