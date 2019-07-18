package pip

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
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

func (p *Pip) List() ([]Requirement, *errors.Error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: p.Cmd,
		Argv: []string{"list", "--format=json"},
	})
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            "shell",
			Troubleshooting: fmt.Sprintf("Ensure that %s is installed, if it is then try to run %s list --format=json", p.Cmd, p.Cmd),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/python.md#strategy-string",
		}
	}
	var reqs []Requirement
	err = json.Unmarshal([]byte(stdout), &reqs)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            "shell",
			Troubleshooting: fmt.Sprintf("the following output from the command %s list --format=json could not be un-marshalled:\n%s\ntry running the command on your own and checking for any errors", p.Cmd, string(stdout)),
			Link:            "https://pip.pypa.io/en/stable/reference/pip_list",
		}
	}
	return reqs, nil
}

// FromFile reads a list of dependencies from the supplied `requirements.txt` formatted file.
func FromFile(filename string) ([]Requirement, *errors.Error) {
	contents, err := files.Read(filename)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            "user",
			Troubleshooting: fmt.Sprintf("Ensure that `%s` exists", filename),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/python.md#analysis",
		}
	}

	var reqs []Requirement
	for _, line := range strings.Split(string(contents), "\n") {
		// Remove all line comments and whitespace.
		commentSplit := strings.Split(line, "#")
		trimmed := strings.TrimSpace(commentSplit[0])
		if strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "-") || trimmed == "" {
			continue
		}

		log.WithField("line", line).Debug("parsing line")
		// See https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
		// and https://pip.pypa.io/en/stable/reference/pip_install/#pip-install-examples
		matched := false
		operators := []string{"===", "<=", ">=", "==", ">", "<", "!=", "~="}
		for _, op := range operators {
			sections := strings.Split(trimmed, op)
			if len(sections) == 2 {
				reqs = append(reqs, Requirement{
					Name:     checkForExtra(sections[0]),
					Revision: sections[1],
					Operator: op,
				})
				matched = true
				break
			}
		}
		if !matched {
			reqs = append(reqs, Requirement{
				Name: checkForExtra(trimmed),
			})
		}
	}

	return reqs, nil
}

// https://www.python.org/dev/peps/pep-0508/#extras
func checkForExtra(name string) string {
	if strings.HasSuffix(name, "]") {
		i := strings.Index(name, "[")
		if i > 0 {
			return name[:i]
		}
	}
	return name
}
