package carthage

import (
	"github.com/fossas/fossa-cli/exec"
)

type Carthage struct {
	Bin string
}

func (c *Carthage) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: c.Bin,
		Argv: []string{"update"},
		Dir:  dir,
	})
	return err
}
