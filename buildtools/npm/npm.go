package npm

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
)

type NPM struct {
	Cmd      string
	AllowErr bool
}

type Output struct {
	Version      string
	From         string
	Resolved     string
	Dependencies map[string]Output
}

func (n *NPM) List(dir string) (Output, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: n.Cmd,
		Argv: []string{"ls", "--json"},
		Dir:  dir,
	})
	log.Logger.Debugf("err: %#v", err)
	log.Logger.Debugf("AllowErr: %#v", n.AllowErr)
	if err != nil && !n.AllowErr {
		return Output{}, err
	}
	var output Output
	err = json.Unmarshal([]byte(stdout), &output)
	if err != nil {
		return Output{}, err
	}
	return output, nil
}

func (n *NPM) Clean(dir string) error {
	return files.Rm(dir, "node_modules")
}

func (n *NPM) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: n.Cmd,
		Argv: []string{"install", "--production"},
		Dir:  dir,
	})
	if err != nil && !n.AllowErr {
		return err
	}
	return nil
}
