package cocoapods

import (
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
)

type Cocoapods struct {
	Bin string
}

func (c *Cocoapods) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: c.Bin,
		Argv: []string{"install"},
		Dir:  dir,
	})
	return err
}

type RawLockfile struct {
	Pods            []interface{}                `yaml:"PODS"`         // Transitive dependencies (this is actually `[](map[string][]string | string)`)
	Dependencies    []string                     `yaml:"DEPENDENCIES"` // Direct dependencies
	CheckoutOptions map[string]map[string]string `yaml:"CHECKOUT OPTIONS"`
	ExternalSources map[string]map[string]string `yaml:"EXTERNAL SOURCES"`
	SpecRepos       map[string][]string          `yaml:"SPEC REPOS"`
}

type Lockfile struct {
	Pods            []Pod
	Dependencies    []Requirement
	CheckoutOptions map[string]CheckoutOption
	ExternalSources map[string]ExternalSource
	SpecRepos       map[string][]string
}

type Pod struct {
	Name         string
	Version      string
	Dependencies []Requirement
}

type Requirement struct {
	Name    string
	Version string
}

type CheckoutOption struct {
	Git    string
	Commit string
}

type ExternalSource struct {
	Git    string
	Branch string
	Tag    string

	Path string
}

func FromLockfile(filename string) (Lockfile, error) {
	var raw RawLockfile
	err := files.ReadYAML(&raw, filename)
	return raw, err
}
