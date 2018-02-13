package build

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"

	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var composerLogger = logging.MustGetLogger("composer")

// ComposerPackage implements Dependency for Composer
type ComposerPackage struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns comp for ComposerPackage
func (m ComposerPackage) Fetcher() string {
	return "comp"
}

// Package returns the package spec for ComposerPackage
func (m ComposerPackage) Package() string {
	return m.Name
}

// Revision returns the version spec for ComposerPackage
func (m ComposerPackage) Revision() string {
	return m.Version
}

// ComposerBuilder implements Builder for Composer (composer.json) builds
type ComposerBuilder struct {
	ComposerCmd     string
	ComposerVersion string

	PhpCmd     string
	PhpVersion string
}

func (builder *ComposerBuilder) Initialize() error {
	builder.ComposerCmd = string(os.Getenv("COMPOSER_BINARY"))
	if builder.ComposerCmd == "" {
		builder.ComposerCmd = "composer"
	}
	outComposerVersion, err := exec.Command(builder.ComposerCmd, "-V").Output()
	if err != nil {
		return fmt.Errorf("unable to get composer version: %s", err)
	}
	outputMatchRe := regexp.MustCompile(`Composer version ([0-9]+\.[0-9]+\.[0-9]+)`)
	match := outputMatchRe.FindStringSubmatch(strings.TrimSpace(string(outComposerVersion)))
	if len(match) == 2 {
		builder.ComposerVersion = match[1]
	}

	builder.PhpCmd = string(os.Getenv("PHP_BINARY"))
	if builder.PhpCmd == "" {
		builder.PhpCmd = "php"
	}
	outPhpVersion, err := exec.Command(builder.PhpCmd, "-v").Output()
	if err != nil {
		return fmt.Errorf("unable to get PHP version: %s", err)
	}
	outputPhpMatchRe := regexp.MustCompile(`PHP ([0-9]+\.[0-9]+\.[0-9]+)`)
	match = outputPhpMatchRe.FindStringSubmatch(strings.TrimSpace(string(outPhpVersion)))
	if len(match) == 2 {
		builder.PhpVersion = match[1]
	}

	if builder.PhpCmd == "" || builder.PhpVersion == "" {
		return errors.New("could not find PHP binary (try setting $PHP_BINARY)")
	}

	if builder.ComposerCmd == "" || builder.ComposerVersion == "" {
		return errors.New("could not find Composer binary (try setting $COMPOSER_BINARY)")
	}

	composerLogger.Debugf("Initialized Composer builder: %#v\n", builder)

	return nil
}

func (builder *ComposerBuilder) Build(m module.Module, force bool) error {
	if force {
		composerLogger.Debug("`force` flag is set; clearing `vendor`...\n")
		cmd := exec.Command("rm", "-rf", "vendor")
		cmd.Dir = m.Dir
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("unable to clear `vendor` folder: %s", err.Error())
		}
	}

	cmd := exec.Command(builder.ComposerCmd, "install", "--prefer-dist", "--no-dev")
	var stderrBuffer bytes.Buffer
	cmd.Dir = m.Dir
	cmd.Stderr = &stderrBuffer
	_, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("unable to run composer build: %s (message: %s)", err.Error(), stderrBuffer.String())
	}
	return nil
}

func (builder *ComposerBuilder) Analyze(m module.Module, _ bool) ([]module.Dependency, error) {
	cmd := exec.Command(builder.ComposerCmd, "show", "-f", "json", "--no-ansi")
	cmd.Dir = m.Dir
	composerShowOutput, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("unable to list composer dependencies: %s", err.Error())
	}

	composerOutData := map[string][]ComposerPackage{}
	err = json.Unmarshal(composerShowOutput, &composerOutData)
	if err != nil {
		return nil, fmt.Errorf("unable to parse composer dependency list output: %s", err.Error())
	}

	var deps []module.Dependency
	for _, d := range composerOutData["installed"] {
		deps = append(deps, d)
	}

	composerLogger.Debugf("Composer dependencies: %#v\n", deps)

	return deps, nil
}

func (builder *ComposerBuilder) IsBuilt(m module.Module, _ bool) (bool, error) {
	composerLogger.Debugf("Checking whether %s is built...\n", m.Name)
	cmd := exec.Command(builder.ComposerCmd, "show", "--no-ansi")
	cmd.Dir = m.Dir

	output, err := cmd.Output()
	if err != nil {
		return false, fmt.Errorf("unable to list installed composer dependencies: %s", err.Error())
	}

	isBuilt := len(strings.TrimSpace(string(output))) > 0
	composerLogger.Debugf("Module IsBuilt?: %t\n", isBuilt)
	return isBuilt, nil
}

func (builder *ComposerBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for ComposerBuilder")
}

func (builder *ComposerBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for ComposerBuilder")
}
