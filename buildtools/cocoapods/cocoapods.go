package cocoapods

import (
	"github.com/fossas/fossa-cli/exec"
)

type Cocoapods struct {
	Bin string
}

func (c *Cocoapods) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: c.Bin,
		Argv: []string{"install"},
		Dir:  dir,
	})
	return err
}
