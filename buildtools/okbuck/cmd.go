package okbuck

import (
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

func Cmd(args ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "./buckw",
		Argv: args,
	})

	if err != nil {
		return out, errors.Wrapf(err, "Could not run `buckw %+v` within the current directory", args)
	}
	return out, nil
}
