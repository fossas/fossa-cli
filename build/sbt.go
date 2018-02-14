package build

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/module"
	logging "github.com/op/go-logging"
)

var sbtLogger = logging.MustGetLogger("sbt")

// SBTLibrary implements Dependency for SBT library dependencies
type SBTLibrary struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns mvn for SBTLibrary
func (m SBTLibrary) Fetcher() string {
	// SBT uses the Maven repositories by default, but it looks like there's also
	// support for Ivy formats + "unmanaged dependencies" (vendored JARs?).
	// TODO: add support for these other formats.
	return "mvn"
}

// Package returns the package spec for SBTLibrary
func (m SBTLibrary) Package() string {
	return m.Name
}

// Revision returns the version spec for SBTLibrary
func (m SBTLibrary) Revision() string {
	return m.Version
}

// SBTBuilder implements build context for SBT builds
type SBTBuilder struct {
	SBTCmd     string
	SBTVersion string

	JavaCmd     string
	JavaVersion string
}

// Initialize collects environment data for SBT builds
func (builder *SBTBuilder) Initialize() error {
	sbtLogger.Debugf("Initializing SBT builder...")

	builder.SBTCmd = string(os.Getenv("SBT_BINARY"))
	if builder.SBTCmd == "" {
		builder.SBTCmd = "sbt"
	}
	sbtAboutOutput, err := exec.Command(builder.SBTCmd, "-no-colors", "about").Output()
	if err != nil {
		return err
	}

	lines := strings.Split(string(sbtAboutOutput), "\n")
	matcher := regexp.MustCompile(`^\[info\] This is sbt (.*?)$`)
	for _, line := range lines {
		line = strings.TrimSpace(line)
		matches := matcher.FindStringSubmatch(line)
		if len(matches) == 2 {
			builder.SBTVersion = matches[1]
		}
	}

	builder.JavaCmd = string(os.Getenv("JAVA_BINARY"))
	if builder.JavaCmd == "" {
		builder.JavaCmd = "java"
	}
	var javaVersionOutput bytes.Buffer
	cmd := exec.Command(builder.JavaCmd, "-version")
	cmd.Stderr = &javaVersionOutput
	_, err = cmd.Output()
	if err != nil {
		return err
	}
	builder.JavaVersion = javaVersionOutput.String()

	if builder.SBTCmd == "" || builder.SBTVersion == "" {
		return errors.New("could not find sbt (try setting $SBT_BINARY)")
	}

	sbtLogger.Debugf("Initialized SBT builder: %#v", builder)
	return nil
}

func (builder *SBTBuilder) Build(m module.Module, force bool) error {
	sbtLogger.Debugf("Running SBT build on %#v", m)
	if force {
		sbtLogger.Debugf("`force` flag is set; running `%s clean`...", builder.SBTCmd)
		cmd := exec.Command(builder.SBTCmd, "clean")
		cmd.Dir = m.Dir
		output, err := cmd.Output()
		if err != nil {
			sbtLogger.Debugf("`%s clean` failed; output: %#v", builder.SBTCmd, string(output))
			return err
		}
		sbtLogger.Debugf("Done running `%s clean`.", builder.SBTCmd)
	}

	sbtLogger.Debugf("Running `%s compile`...", builder.SBTCmd)
	cmd := exec.Command(builder.SBTCmd, "compile")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	if err != nil {
		sbtLogger.Debugf("`%s compile` failed; output: %#v", builder.SBTCmd, string(output))
		return err
	}
	sbtLogger.Debugf("Done running `%s compile`.", builder.SBTCmd)

	sbtLogger.Debugf("Done running SBT build.")
	return nil
}

func (builder *SBTBuilder) Analyze(m module.Module, _ bool) ([]module.Dependency, error) {
	sbtLogger.Debugf("Running SBT analysis on %#v", m)
	sbtLogger.Debugf("Running `%s -no-colors dependencyList`...", builder.SBTCmd)
	cmd := exec.Command(builder.SBTCmd, "-no-colors", "dependencyList")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	outStr := string(output)
	if err != nil {
		sbtLogger.Debugf("`%s -no-colors dependencyList` failed; output: %#v", builder.SBTCmd, outStr)
		return nil, err
	}
	sbtLogger.Debugf("Done running `%s -no-colors dependencyList`.", builder.SBTCmd)

	sbtLogger.Debugf("Parsing SBT output...")
	deps := []module.Dependency{}
	for _, line := range strings.Split(outStr, "\n") {
		if strings.Index(line, "[info] Loading ") != -1 ||
			strings.Index(line, "[info] Resolving ") != -1 ||
			strings.Index(line, "[info] Set ") != -1 ||
			strings.Index(line, "[info] Updating ") != -1 ||
			strings.Index(line, "[info] Done ") != -1 ||
			strings.Index(line, "[info] ") != 0 {
			sbtLogger.Debugf("Ignoring line: %#v", line)
			continue
		}
		sbtLogger.Debugf("Matched line: %#v", line)

		if len(line) > 0 {
			depLocator := strings.TrimSpace(strings.TrimPrefix(line, "[info] "))
			depSections := strings.Split(depLocator, ":")
			dep := module.Dependency(SBTLibrary{
				Name:    depSections[0] + ":" + depSections[1],
				Version: depSections[2],
			})
			sbtLogger.Debugf("Adding dep: %#v", dep)
			deps = append(deps, dep)
		}
	}
	sbtLogger.Debugf("Done parsing SBT output.")

	sbtLogger.Debugf("Done running SBT analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *SBTBuilder) IsBuilt(m module.Module, _ bool) (bool, error) {
	sbtLogger.Debugf("Checking SBT IsBuilt on %#v", m)

	sbtLogger.Debugf("Running `%s -no-colors dependencyList`...", builder.SBTCmd)
	cmd := exec.Command(builder.SBTCmd, "-no-colors", "dependencyList")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	outStr := string(output)
	if err != nil {
		sbtLogger.Debugf("`%s -no-colors dependencyList` failed; output: %#v", builder.SBTCmd, outStr)
		return false, err
	}
	sbtLogger.Debugf("Done running `%s -no-colors dependencyList`.", builder.SBTCmd)

	isBuilt := outStr != ""
	sbtLogger.Debugf("Done checking SBT IsBuilt: %#v", isBuilt)

	return isBuilt, nil
}

func (builder *SBTBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for SBTBuilder")
}

func (builder *SBTBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for SBTBuilder")
}
