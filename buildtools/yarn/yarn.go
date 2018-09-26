package yarn

import "github.com/fossas/fossa-cli/exec"

type Yarn interface {
	// TODO: implement yarn list --production. Skipping for now because the implementation relies 100% on yarn.lock
	// List(dir string) error
	Install(dir string) error
}

type SystemYarn struct {
	Cmd string
}

func (y SystemYarn) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: y.Cmd,
		Argv: []string{"install", "--production"},
		Dir:  dir,
	})

	return err
}
