package v2

import (
	"github.com/fossas/fossa-cli/module"
)

type File struct {
	Version int `yaml:"version"`

	Endpoint string `yaml:"server,omitempty"`
	Project  string `yaml:"project,omitempty"`
	Revision string `yaml:"revision,omitempty"`
	Branch   string `yaml:"branch,omitempty"`
	Imported bool   `yaml:"imported,omitempty"`

	Modules []module.Module
}
