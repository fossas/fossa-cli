package exec

import (
	"bytes"
	"os"
	"os/exec"

	"github.com/fossas/fossa-cli/log"
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

// func RunJSON(v interface{}, cmd Cmd) (stderr string, err error) {}

// Run executes a `Cmd`.
func Run(cmd Cmd) (stdout, stderr string, err error) {
	log.Logger.Debugf("Running command: %#v", append([]string{cmd.Name}, cmd.Argv...))

	var stderrBuffer bytes.Buffer
	xc := exec.Command(cmd.Name, cmd.Argv...)
	xc.Stderr = &stderrBuffer

	if cmd.Dir != "" {
		xc.Dir = cmd.Dir
	}

	if cmd.Env != nil {
		xc.Env = toEnv(cmd.Env)
	} else if cmd.WithEnv != nil {
		xc.Env = append(xc.Env, toEnv(cmd.Env)...)
		xc.Env = append(xc.Env, os.Environ()...)
	} else {
		xc.Env = os.Environ()
	}

	log.Logger.Debugf("Running in directory: %s", xc.Dir)
	log.Logger.Debugf("Running with environment: %#v", xc.Env)

	stdoutBuffer, err := xc.Output()
	stdout = string(stdoutBuffer)
	stderr = stderrBuffer.String()

	log.Logger.Debugf("STDOUT: %#v", stdout)
	log.Logger.Debugf("STDERR: %#v", stderr)
	log.Logger.Debugf("Done running.")

	return stdout, stderr, err
}

func toEnv(env map[string]string) []string {
	var out []string
	for key, val := range env {
		out = append(out, key+"="+val)
	}
	return out
}
