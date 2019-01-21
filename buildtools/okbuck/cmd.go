package okbuck

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

func Cmd(cmd string, args ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "./buckw",
		Argv: append([]string{cmd}, args...),
	})

	if err != nil {
		return out, errors.Wrapf(err, "Could not run `buckw %s %+v` within the current directory", cmd, args)
	}
	return out, nil
}
