package pip

import (
	"encoding/json"
	"fmt"
	"strings"
	"unicode"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
)

type Pip struct {
	Cmd       string
	PythonCmd string
}

// Structure for deserializing the output of `pip list`
type listRequirement struct {
	Name     string `json:"name"`
	Revision string `json:"version"`
}

type Requirement struct {
	Name        string
	Constraints []Constraint
}

type Constraint struct {
	Revision string
	Operator string
}

func (c Constraint) String() string {
	return c.Operator + c.Revision
}

func (r Requirement) String() string {
	var constraintStrings []string
	for _, constraint := range r.Constraints {
		constraintStrings = append(constraintStrings, constraint.String())
	}
	return r.Name + strings.Join(constraintStrings, ",")
}

func (p *Pip) Install(requirementsFilename string) *errors.Error {
	_, _, err := exec.Run(exec.Cmd{
		Name: p.Cmd,
		Argv: []string{"install", "-r", requirementsFilename},
	})

	return &errors.Error{
		Cause:           err,
		Type:            errors.Exec,
		Troubleshooting: fmt.Sprintf("Fossa could not run `%s install -r %s`. Try running this command and ensure that %s is installed in your environment.", p.Cmd, requirementsFilename, p.Cmd),
		Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/python.md#strategy-string",
	}
}

func (p *Pip) List() ([]Requirement, *errors.Error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: p.Cmd,
		Argv: []string{"list", "--format=json"},
	})
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Ensure that %s is installed, if it is then try to run %s list --format=json", p.Cmd, p.Cmd),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/python.md#strategy-string",
		}
	}
	var listReqs []listRequirement
	err = json.Unmarshal([]byte(stdout), &listReqs)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The following output from the command %s list --format=json could not be un-marshalled into JSON:\n%s\ntry running the command on your own and check for any errors", p.Cmd, string(stdout)),
			Link:            "https://pip.pypa.io/en/stable/reference/pip_list",
		}
	}

	var reqs []Requirement
	for _, listReq := range listReqs {
		reqs = append(reqs, Requirement{
			Name: listReq.Name,
			Constraints: []Constraint{{Revision: listReq.Revision}},
		})
	}
	return reqs, nil
}

const requiresField = "install_requires"

func FromSetupPy(filename string) ([]Requirement, *errors.Error) {
	raw, err := files.Read(filename)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.User,
			Troubleshooting: fmt.Sprintf("Ensure that `%s` exists.", filename),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/python.md#analysis",
		}
	}

	contents := string(raw)

	reqStart := strings.Index(contents, requiresField)
	if reqStart == -1 {
		return nil, nil
	}

	contents = contents[(reqStart + len(requiresField)):]

	openBracket := strings.Index(contents, "[")
	if openBracket == -1 {
		return nil, &errors.Error{
			Type: errors.Exec,
			Message: "Failed to parse setup.py: expected to find '[' after \"" + requiresField + "\"",
		}
	}

	contents = contents[openBracket + 1:]

	endBracket := strings.Index(contents, "]")
	if endBracket == -1 {
		return nil, &errors.Error{
			Type: errors.Exec,
			Message: "Failed to parse setup.py: expected to find ']' after \"" + requiresField + "\"",
		}
	}

	var reqs []Requirement
	for {
		endBracket := strings.Index(contents, "]")
		openQuote := strings.Index(contents, "'")
		if openQuote == -1 || openQuote > endBracket {
			break
		}

		contents = contents[openQuote + 1:]
		closeQuote := strings.Index(contents, "'")
		if closeQuote == -1 {
			// NB: silently failing here instead of throwing an error
			break
		}

		depString := contents[:closeQuote]
		contents = contents[closeQuote + 1:]

		reqs = append(reqs, parseRequirement(depString))
	}

	return reqs, nil
}

// FromFile reads a list of dependencies from the supplied `requirements.txt` formatted file.
func FromFile(filename string) ([]Requirement, *errors.Error) {
	contents, err := files.Read(filename)
	if err != nil {
		return nil, &errors.Error{
			Cause:           err,
			Type:            errors.User,
			Troubleshooting: fmt.Sprintf("Ensure that `%s` exists.", filename),
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
		req := parseRequirement(trimmed)
		req.Name = checkForExtra(req.Name)
		reqs = append(reqs, req)
	}

	return reqs, nil
}

func parseRequirement(raw string) Requirement {
	// See https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
	// and https://pip.pypa.io/en/stable/reference/pip_install/#pip-install-examples
	trimmed := strings.ReplaceAll(raw, " ", "")

	constraintsStart := -1
	for ix, char := range trimmed {
		// operators start with these characters
		if char == '~' || char == '=' || char == '>' || char == '<' || char == '!' {
			constraintsStart = ix
			break
		}
	}

	name := trimmed
	var constraints []Constraint

	if constraintsStart != -1 {
		name = trimmed[:constraintsStart]
		constraintsRaw := strings.Split(trimmed[constraintsStart:], ",")
		for _, constraintRaw := range constraintsRaw {
			constraints = append(constraints, parseConstraint(constraintRaw))
		}
	}

	return Requirement{
		Name: name,
		Constraints: constraints,
	}
}

func parseConstraint(raw string) Constraint {
	for ix, char := range raw {
		if unicode.IsDigit(char) {
			return Constraint{
				Revision: raw[ix:],
				Operator: raw[:ix],
			}
		}
	}
	return Constraint{
		Revision: raw,
	}
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
