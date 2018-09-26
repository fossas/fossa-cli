package exec

import (
	"bytes"
	"os"
	"os/exec"

	"github.com/apex/log"
)

// Cmd represents a single command. If Name and Argv are set, this is treated as
// an executable. If Command is set, this is treated as a shell command.
type Cmd struct {
	Name    string   // Executable name.
	Argv    []string // Executable arguments.
	Command string   // Shell command.

	Dir string // The Command's working directory.

	// If neither Env nor WithEnv are set, the environment is inherited from os.Environ().
	Env     map[string]string // If set, the command's environment is _set_ to Env.
	WithEnv map[string]string // If set, the command's environment is _added_ to WithEnv.
}

// Run executes a `Cmd`.
func Run(cmd Cmd) (stdout, stderr string, err error) {
	log.WithFields(log.Fields{
		"name": cmd.Name,
		"argv": cmd.Argv,
	}).Debug("called Run")

	xc, stderrBuf := BuildExec(cmd)

	log.WithFields(log.Fields{
		"dir": xc.Dir,
		"env": xc.Env,
	}).Debug("executing command")

	stdoutBuf, err := xc.Output()
	stdout = string(stdoutBuf)
	stderr = stderrBuf.String()

	log.WithFields(log.Fields{
		"stdout": stdout,
		"stderr": stderr,
	}).Debug("done running")

	return stdout, stderr, err
}

func toEnv(env map[string]string) []string {
	var out []string
	for key, val := range env {
		out = append(out, key+"="+val)
	}
	return out
}

// BuildExec translates FOSSA exec structures into standard library exec
// commands.
func BuildExec(cmd Cmd) (*exec.Cmd, bytes.Buffer) {
	var stderr bytes.Buffer
	xc := exec.Command(cmd.Name, cmd.Argv...)
	xc.Stderr = &stderr

	if cmd.Dir != "" {
		xc.Dir = cmd.Dir
	}

	if cmd.Env != nil {
		xc.Env = toEnv(cmd.Env)
	} else if cmd.WithEnv != nil {
		xc.Env = append(xc.Env, toEnv(cmd.WithEnv)...)
		xc.Env = append(xc.Env, os.Environ()...)
	} else {
		xc.Env = os.Environ()
	}

	return xc, stderr
}
