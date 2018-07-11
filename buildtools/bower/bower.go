package bower

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/files"

	"github.com/fossas/fossa-cli/exec"
)

type PackageMetadata struct {
	Name    string
	Version string

	TargetName    string `json:"_originalSource"`
	TargetVersion string `json:"_target"`
}

type Endpoint struct {
	Name   string
	Source string
	Target string
}

type Package struct {
	Endpoint     Endpoint
	PkgMeta      PackageMetadata
	Dependencies map[string]Package
	Missing      bool
}

type Bower struct {
	Cmd    string
	Config Config
}

func New(cmd, dir string) (*Bower, error) {
	config, err := ReadConfig(dir)
	if err != nil {
		return nil, err
	}
	bower := Bower{
		Cmd:    cmd,
		Config: config,
	}
	return &bower, nil
}

func (b *Bower) List() (Package, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: []string{"list", "--json"},
		Dir:  b.Config.CWD,
	})
	if err != nil {
		return Package{}, err
	}
	var pkg Package
	err = json.Unmarshal([]byte(stdout), &pkg)
	if err != nil {
		return Package{}, err
	}
	return pkg, nil
}

func (b *Bower) Clean() error {
	return files.Rm(b.Config.Directory)
}

func (b *Bower) Install(production bool) error {
	args := []string{"install"}
	if production {
		args = append(args, "--production")
	}
	_, _, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: args,
		Dir:  b.Config.CWD,
	})
	return err
}
