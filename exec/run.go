package exec

import (
	"bytes"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/errors"
)

// Cmd represents a single command. If Name and Argv are set, this is treated as
// an executable. If Command is set, this is treated as a shell command.
type Cmd struct {
	Name    string   // Executable name.
	Argv    []string // Executable arguments.
	Command string   // Shell command.
	Dir     string   // The Command's working directory.

	Timeout string // Specifies the amount of time a command is allowed to run.
	Retries int    // Amount of times a command can be retried.

	// If neither Env nor WithEnv are set, the environment is inherited from os.Environ().
	Env     map[string]string // If set, the command's environment is _set_ to Env.
	WithEnv map[string]string // If set, the command's environment is _added_ to WithEnv.
}

// Run executes a `Cmd`, retries the specified amount, and checks for command timeout if specified.
func Run(cmd Cmd) (string, string, error) {
	var stdout, stderr string
	var err error

	for i := 0; i <= cmd.Retries; i++ {
		if cmd.Timeout != "" {
			stdout, stderr, err = runWithTimeout(cmd)
		} else {
			log.WithFields(log.Fields{
				"name": cmd.Name,
				"argv": cmd.Argv,
			}).Debug("called Run")

			xc, stderrBuf := BuildExec(cmd)

			log.WithFields(log.Fields{
				"dir": xc.Dir,
				"env": xc.Env,
			}).Debug("executing command")

			var stdoutBuf []byte
			stdoutBuf, err = xc.Output()
			stdout = string(stdoutBuf)
			stderr = stderrBuf.String()

			log.WithFields(log.Fields{
				"stdout": stdout,
				"stderr": stderr,
			}).Debug("done running")
		}

		if err == nil {
			break
		}
	}

	return stdout, stderr, err
}

// RunTimeout executes a `Cmd` and waits to see if it times out.
func runWithTimeout(cmd Cmd) (string, string, error) {
	log.WithFields(log.Fields{
		"name": cmd.Name,
		"argv": cmd.Argv,
	}).Debug("called Start")

	xc, stderr := BuildExec(cmd)
	var stdout strings.Builder
	xc.Stdout = &stdout

	err := xc.Start()
	if err != nil {
		return "", "", errors.Wrap(err, "error starting command")
	}

	log.WithFields(log.Fields{
		"dir": xc.Dir,
		"env": xc.Env,
	}).Debug("executing command")

	done := make(chan error)
	go func() {
		done <- xc.Wait()
	}()

	timeout, err := time.ParseDuration((cmd.Timeout))
	if err != nil {
		return "", "", errors.Wrap(err, "unable to determine timeout value")
	}

	select {
	case <-time.After(timeout):
		err := xc.Process.Kill()
		if err != nil {
			return "", "", errors.Wrapf(err, "error killing the process")
		}

		return "", "", errors.Errorf("operation timed out running `%s %s` after %s", cmd.Name, cmd.Argv, cmd.Timeout)
	case err := <-done:
		if err != nil {
			return "", "", errors.Wrap(err, "error waiting for command to finish")
		}

		log.WithFields(log.Fields{
			"stdout": stdout.String(),
			"stderr": stderr.String(),
		}).Debug("done running")
		return stdout.String(), stderr.String(), nil
	}

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
