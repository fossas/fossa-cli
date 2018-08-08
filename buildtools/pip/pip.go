package pip

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/apex/log"
)

// TODO: add a Python sidecar that evaluates `setup.py`.

type Pip struct {
	Cmd       string
	PythonCmd string
}

type Requirement struct {
	Name     string `json:"name"`
	Revision string `json:"version"`
	Operator string
}

func (r Requirement) String() string {
	return r.Name + r.Operator + r.Revision
}

func (p *Pip) Install(requirementsFilename string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: p.Cmd,
		Argv: []string{"install", "-r", requirementsFilename},
	})
	return err
}

func (p *Pip) List() ([]Requirement, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: p.Cmd,
		Argv: []string{"list", "--format=json"},
	})
	if err != nil {
		return nil, err
	}
	var reqs []Requirement
	err = json.Unmarshal([]byte(stdout), &reqs)
	if err != nil {
		return nil, err
	}
	return reqs, nil
}

func FromFile(filename string) ([]Requirement, error) {
	contents, err := files.Read(filename)
	if err != nil {
		return nil, err
	}

	var reqs []Requirement
	for _, line := range strings.Split(string(contents), "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "#") || trimmed == "" {
			continue
		}
		log.Logger.Debugf("line: %#v", line)
		// See https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
		// and https://pip.pypa.io/en/stable/reference/pip_install/#pip-install-examples
		matched := false
		operators := []string{"<=", ">=", "==", ">", "<", "!="}
		for _, op := range operators {
			sections := strings.Split(trimmed, op)
			if len(sections) == 2 {
				reqs = append(reqs, Requirement{
					Name:     sections[0],
					Revision: sections[1],
					Operator: op,
				})
				matched = true
				break
			}
		}
		if !matched {
			reqs = append(reqs, Requirement{
				Name: trimmed,
			})
		}
	}

	return reqs, nil
}
