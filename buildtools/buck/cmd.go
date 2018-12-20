package buck

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

func buckAudit(cmd, target string, args ...string) (AuditOutput, error) {
	var output AuditOutput
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{"audit", cmd, "--json", target}, args...),
	})
	if err != nil {
		return output, errors.Wrapf(err, "Could not run `buck audit %s --json %s %+v` within the current directory", cmd, target, args)
	}

	err = json.Unmarshal([]byte(out), &output.OutputMapping)
	if err != nil {
		return output, errors.Wrap(err, "Could not unmarshal `buck audit` JSON into dependency list")
	}

	return output, nil
}

func buckRoot() (string, error) {
	root, err := buckCmd("root")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(root), nil
}

func buckCmd(cmd string, args ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{cmd}, args...),
	})

	if err != nil {
		return out, errors.Wrapf(err, "Could not run `buck %s %+v` within the current directory", cmd, args)
	}
	return out, nil
}
