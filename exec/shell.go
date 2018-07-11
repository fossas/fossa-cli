package exec

import "os"

func Shell(cmd Cmd) (stdout, stderr string, err error) {
	execCmd := cmd
	execCmd.Name = os.Getenv("SHELL")
	execCmd.Argv = []string{"-c", cmd.Command}
	return Run(execCmd)
}
