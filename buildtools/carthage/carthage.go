package carthage

import (
	"fmt"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

type Carthage struct {
	Bin string
}

func (c *Carthage) Install(dir string) *errors.Error {
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: c.Bin,
		Argv: []string{"update"},
		Dir:  dir,
	})
	if err != nil {
		return &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("Fossa could not run `%s update` within the directory %s. Try running this command and ensure that %s is installed in your environment.\nstdout: %s\nstderr: %s", c.Bin, dir, c.Bin, stdout, stderr),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/carthage.md#support",
		}
	}
	return nil
}
