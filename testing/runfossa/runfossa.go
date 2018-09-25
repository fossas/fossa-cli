package runfossa

import "github.com/fossas/fossa-cli/exec"

// Init executes fossa init the provided directory
func Init(projectDir string) error {
	cmd := exec.Cmd{
		Argv:    []string{"init"},
		Name:    "fossa",
		Dir:     projectDir,
		Command: "fossa",
	}

	_, errMsg, err := exec.Run(cmd)
	if err != nil {
		println(errMsg)
	}

	return err
}
