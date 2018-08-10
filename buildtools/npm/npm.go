package npm

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
)

type NPM struct {
	Cmd       string
	StrictErr bool
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
	log.Logger.Debugf("StrictErr: %#v", n.StrictErr)
	if err != nil && n.StrictErr {
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
	if err != nil && n.StrictErr {
		return err
	}
	return nil
}
