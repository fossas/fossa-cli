package buck

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

func Cmd(cmd string, args ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{cmd}, args...),
	})

	if err != nil {
		return out, errors.Wrapf(err, "Could not run `buck %s %+v` within the current directory", cmd, args)
	}
	return out, nil
}

func cmdAudit(command func(string, ...string) (string, error), cmd string, argv ...string) (AuditOutput, error) {
	var output AuditOutput
	arguments := append([]string{cmd, "--json"}, argv...)
	out, err := command("audit", arguments...)
	if err != nil {
		return output, errors.Wrapf(err, "Could not run `buck audit %s --json %+v` within the current directory", cmd, argv)
	}

	err = json.Unmarshal([]byte(out), &output.OutputMapping)
	if err != nil {
		return output, errors.Wrap(err, "Could not unmarshal `buck audit` JSON into dependency list")
	}
	return output, nil
}

func cmdTargets(command func(string, ...string) (string, error), argv ...string) ([]string, error) {
	targets := []string{}
	out, err := command("targets", argv...)
	if err != nil {
		return targets, err
	}

	for _, target := range strings.Split(out, "\n") {
		if len(target) > 0 {
			targets = append(targets, target)
		}
	}
	return targets, nil
}
