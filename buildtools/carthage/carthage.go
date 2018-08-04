package carthage

import (
	"github.com/fossas/fossa-cli/exec"
)

type Carthage struct {
	Bin string
}

type DepTree struct {
	Package      string
	Resolved     string
	Target       string
	Dependencies []DepTree
}

func (c *Carthage) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: c.Bin,
		Argv: []string{"update"},
		Dir:  dir,
	})
	return err
}
