package build

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	logging "github.com/op/go-logging"
)

// Utilities for finding files and manifests
func hasFile(elem ...string) (bool, error) {
	_, err := os.Stat(filepath.Join(elem...))
	if os.IsNotExist(err) {
		return false, nil
	}
	return !os.IsNotExist(err), err
}

func orPredicates(predicates ...stopPredicate) stopPredicate {
	return func(path string) (bool, error) {
		for _, predicate := range predicates {
			ok, err := predicate(path)
			if err != nil {
				return false, err
			}
			if ok {
				return ok, nil
			}
		}
		return false, nil
	}
}

type stopPredicate func(path string) (bool, error)

func findAncestor(stopWhen stopPredicate, path string) (string, bool, error) {
	absPath, err := filepath.Abs(path)
	if absPath == string(filepath.Separator) {
		return "", false, nil
	}
	if err != nil {
		return "", false, err
	}
	stop, err := stopWhen(absPath)
	if err != nil {
		return "", false, err
	}
	if stop {
		return absPath, true, nil
	}
	return findAncestor(stopWhen, filepath.Dir(path))
}

// Utilities around `exec.Command`
func run(name string, arg ...string) (string, string, error) {
	var stderr bytes.Buffer
	cmd := exec.Command(name, arg...)
	cmd.Stderr = &stderr
	stdout, err := cmd.Output()
	return string(stdout), stderr.String(), err
}

func runInDir(dir string, name string, arg ...string) (string, string, error) {
	var stderr bytes.Buffer
	cmd := exec.Command(name, arg...)
	cmd.Dir = dir
	cmd.Stderr = &stderr
	stdout, err := cmd.Output()
	return string(stdout), stderr.String(), err
}

func runLogged(logger *logging.Logger, dir string, name string, arg ...string) (string, string, error) {
	stdout, stderr, err := runInDir(dir, name, arg...)
	if err != nil {
		cmd := strings.Join(append([]string{name}, arg...), " ")
		logger.Debugf("Running `%s` failed: %#v %#v", cmd, err, stderr)
		return "", "", fmt.Errorf("running `%s` failed: %#v %#v", cmd, err, stderr)
	}
	return stdout, stderr, nil
}

// Utilities for detecting which binary to use
type versionResolver func(cmd string) (string, error)

func whichWithResolver(cmds []string, getVersion versionResolver) (string, string, error) {
	for _, cmd := range cmds {
		version, err := getVersion(cmd)
		if err == nil {
			return cmd, version, nil
		}
	}
	return "", "", errors.New("could not resolve version")
}

func which(versionFlags string, cmds ...string) (string, string, error) {
	return whichWithResolver(cmds, func(cmd string) (string, error) {
		version, _, err := run(cmd, strings.Split(versionFlags, " ")...)
		if err != nil {
			return "", err
		}
		return version, nil
	})
}
