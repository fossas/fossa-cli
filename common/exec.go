package common

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

type execImpl struct {
	logger LogService
}

func newExecService(logger LogService) ExecService {
	return &execImpl{logger: logger}
}

func (executor *execImpl) RunCWD(cmd string, args ...string) (stdout string, stderr string, err error) {
	cwd, err := os.Getwd()
	if err != nil {
		return "", "",
			fmt.Errorf("could not get working directory to run `%s`: %s",
				strings.Join(append([]string{cmd}, args...), " "), err.Error())
	}
	return executor.Run(cwd, cmd, args...)
}

func (executor *execImpl) Run(dir string, cmd string, args ...string) (stdout string, stderr string, err error) {
	name := strings.Join(append([]string{cmd}, args...), " ")
	executor.logger.Debugf("Running `%s` in dir `%s`...", name, dir)

	var stderrBuf bytes.Buffer
	proc := exec.Command(cmd, args...)
	proc.Dir = dir
	proc.Stderr = &stderrBuf
	stdoutBytes, err := proc.Output()
	stdout = string(stdoutBytes)
	stderr = stderrBuf.String()
	if err != nil {
		executor.logger.Debugf("Running `%s` failed: %#v %#v", name, err, stderr)
		return "", "", fmt.Errorf("could not run `%s`: %#v %#v", name, err, stderr)
	}

	executor.logger.Debugf("Done running `%s`: %#v %#v", stdout, stderr)
	return stdout, stderr, nil
}

func (executor *execImpl) Which(args []string, candidates ...string) (cmd string, version string, err error) {
	return executor.WhichWithResolver(func(candidate string) (string, bool) {
		stdout, stderr, err := executor.RunCWD(candidate, args...)
		if err != nil {
			return "", false
		}
		if stdout == "" {
			return stderr, true
		}
		return stdout, true
	}, candidates...)
}

func (executor *execImpl) WhichWithResolver(
	resolve WhichResolver, candidates ...string) (cmd string, version string, err error) {
	for _, candidate := range candidates {
		version, ok := resolve(candidate)
		if ok {
			return candidate, version, nil
		}
		executor.logger.Debugf("Tried resolving `%s` but did not work: %#v %#v", candidate, err, version)
	}
	return "", "", errors.New("could not resolve version")
}
