package bower

import (
	"encoding/json"
	"fmt"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
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

func New(cmd, dir string) (*Bower, *errors.Error) {
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

func (b *Bower) List() (Package, *errors.Error) {
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: []string{"list", "--json"},
		Dir:  b.Config.CWD,
	})
	if err != nil {
		return Package{}, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Ensure that %s is installed, if it is then try to run `%s list --json`\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/bower.md#bower",
		}
	}
	var pkg Package
	err = json.Unmarshal([]byte(stdout), &pkg)
	if err != nil {
		return Package{}, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The following output from the command %s list --json could not be un-marshalled into JSON:\n%s\ntry running the command on your own and check for any errors", b.Cmd, string(stdout)),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/bower.md#bower",
		}
	}
	return pkg, nil
}

func (b *Bower) Clean() *errors.Error {
	err := files.Rm(b.Config.Directory)
	if err != nil {
		return errors.UnknownError(err, fmt.Sprintf("Bower project directory `%s` could not be cleaned", b.Config.Directory))
	}
	return nil
}

func (b *Bower) Install(production bool) *errors.Error {
	args := []string{"install"}
	if production {
		args = append(args, "--production")
	}
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: b.Cmd,
		Argv: args,
		Dir:  b.Config.CWD,
	})
	if err != nil {
		return &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Ensure that %s is installed, if it is then try to run `%s %s --production`\nstdout: %s\nstderr: %s", b.Cmd, b.Cmd, args, stdout, stderr),
			Link:            "https://bower.io/docs/api/#install",
		}
	}
	return nil
}
