package v2

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/module"
)

type File struct {
	Version int `yaml:"version"`

	Endpoint       string `yaml:"server,omitempty"`
	Project        string `yaml:"project,omitempty"`
	Revision       string `yaml:"revision,omitempty"`
	Branch         string `yaml:"branch,omitempty"`
	ProjectURL     string `yaml:"project_url,omitempty"`
	JiraProjectKey string `yaml:"jira_project_key,omitempty"`
	ImportedVCS    bool   `yaml:"imported-vcs,omitempty"`

	Modules []module.Module
}

func New(data []byte) (File, error) {
	return File{}, errors.NotImplementedError()
}
