package yarn

import (
	"os"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/exec"
)

type YarnTool interface {
	// TODO: implement yarn list --production --frozen-lockfile. Skipping for now because the implementation relies 100% on yarn.lock
	// List(dir string) error
	Install(dir string) error
	// Exists returns true if yarn is available
	Exists() bool
}

type SystemYarn struct {
	Cmd string
}

func New() (YarnTool, error) {
	yarnCmd, _, yarnErr := exec.Which("-v", os.Getenv("FOSSA_NPM_CMD"), "yarn")
	if yarnErr != nil {
		log.Warnf("Could not find Yarn %s", yarnErr.Error())
		return SystemYarn{}, yarnErr
	}

	return SystemYarn{
		Cmd: yarnCmd,
	}, nil
}

func (y SystemYarn) Exists() bool {
	return y.Cmd != ""
}

func (y SystemYarn) Install(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: y.Cmd,
		Argv: []string{"install", "--production", "--frozen-lockfile"},
		Dir:  dir,
	})

	return err
}
