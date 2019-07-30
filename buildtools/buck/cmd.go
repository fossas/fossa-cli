package buck

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

// NewCmd creates a function that executes the chosen buck executable.
func NewCmd(name string) func(string, ...string) (string, *errors.Error) {
	return func(cmd string, args ...string) (string, *errors.Error) {
		out, stderr, err := exec.Run(exec.Cmd{
			Name: name,
			Argv: append([]string{cmd}, args...),
		})

		if err != nil {
			return out, &errors.Error{
				Cause:           err,
				Type:            errors.Exec,
				Troubleshooting: fmt.Sprintf("Fossa could not run `%s %s %+v` within the current directory. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", name, cmd, args, name, out, stderr),
				Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/buck.md#support",
			}
		}
		return out, nil
	}
}

func cmdAudit(command func(string, ...string) (string, *errors.Error), cmd string, argv ...string) (AuditOutput, *errors.Error) {
	var output AuditOutput
	arguments := append([]string{cmd, "--json"}, argv...)
	out, err := command("audit", arguments...)
	if err != nil {
		return output, err
	}

	jsonError := json.Unmarshal([]byte(out), &output.OutputMapping)
	if err != nil {
		return output, &errors.Error{
			Cause:           jsonError,
			Type:            errors.Unknown,
			Troubleshooting: fmt.Sprintf("The following output from the command `%s audit --json %s` could not be un-marshalled into JSON:\n%s\ntry running the command on your own and check for any errors", cmd, argv, string(out)),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/buck.md#support",
		}
	}
	return output, nil
}

func cmdTargets(command func(string, ...string) (string, *errors.Error), argv ...string) ([]string, *errors.Error) {
	out, err := command("targets", argv...)
	if err != nil {
		return nil, err
	}

	targets := []string{}
	for _, target := range strings.Split(out, "\n") {
		if len(target) > 0 {
			targets = append(targets, target)
		}
	}
	return targets, nil
}
