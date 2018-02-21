package build

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	logging "github.com/op/go-logging"
)

var commonLogger = logging.MustGetLogger("common")

// Utilities for finding files and manifests
func hasFile(elem ...string) (bool, error) {
	_, err := os.Stat(filepath.Join(elem...))
	if os.IsNotExist(err) {
		return false, nil
	}
	return !os.IsNotExist(err), err
}

func isFile(elem ...string) (bool, error) {
	mode, err := fileMode(elem...)
	if err != nil {
		return false, nil
	}

	return mode.IsRegular(), nil
}

func isFolder(elem ...string) (bool, error) {
	mode, err := fileMode(elem...)
	if err != nil {
		return false, nil
	}

	return mode.IsDir(), nil
}

func fileMode(elem ...string) (os.FileMode, error) {
	file, err := os.Stat(filepath.Join(elem...))
	if err != nil {
		return 0, err
	}

	return file.Mode(), nil
}

func orPredicates(predicates ...fileChecker) fileChecker {
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

type fileChecker func(path string) (bool, error)

func findAncestor(stopWhen fileChecker, path string) (string, bool, error) {
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

// Utilities for debug logging
func runLogged(logger *logging.Logger, dir string, name string, arg ...string) (string, string, error) {
	cmd := strings.Join(append([]string{name}, arg...), " ")
	logger.Debugf("Running `%s`...", cmd)
	stdout, stderr, err := runInDir(dir, name, arg...)
	if err != nil {
		logger.Debugf("Running `%s` failed: %#v %#v", cmd, err, stderr)
		return "", "", fmt.Errorf("running `%s` failed: %#v %#v", cmd, err, stderr)
	}
	logger.Debugf("Done running `%s`: %#v %#v", stdout, stderr)
	return stdout, stderr, nil
}

func parseLogged(logger *logging.Logger, file string, v interface{}) error {
	return parseLoggedWithUnmarshaller(logger, file, v, json.Unmarshal)
}

type unmarshaller func(data []byte, v interface{}) error

func parseLoggedWithUnmarshaller(logger *logging.Logger, file string, v interface{}, unmarshal unmarshaller) error {
	logger.Debugf("Parsing %s...", file)

	contents, err := ioutil.ReadFile(file)
	if err != nil {
		logger.Debugf("Error reading %s: %s", file, err.Error())
		return err
	}
	err = unmarshal(contents, v)
	if err != nil {
		logger.Debugf("Error parsing %s: %#v %#v", file, err, contents)
		return err
	}

	logger.Debugf("Done parsing %s.", file)
	return nil
}

// Utilities for detecting which binary to use
type versionResolver func(cmd string) (string, error)

func whichWithResolver(cmds []string, getVersion versionResolver) (string, string, error) {
	for _, cmd := range cmds {
		version, err := getVersion(cmd)
		if err == nil {
			return cmd, version, nil
		}
		commonLogger.Debugf("Tried resolving `%s` but did not work: %#v %#v", cmd, err, version)
	}
	return "", "", errors.New("could not resolve version")
}

func which(versionFlags string, cmds ...string) (string, string, error) {
	return whichWithResolver(cmds, func(cmd string) (string, error) {
		stdout, stderr, err := run(cmd, strings.Split(versionFlags, " ")...)
		if err != nil {
			return "", err
		}
		if stdout == "" {
			return stderr, nil
		}
		return stdout, nil
	})
}
