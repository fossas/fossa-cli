package build

import (
	"errors"
	"os"
	"os/exec"
	"strings"

	. "github.com/fossas/fossa-cli/log"
)

// SBTContext implements build context for SBT builds
type SBTContext struct {
	SbtCmd     string
	SbtVersion string

	cachedSbtDepListOutput string
}

// SbtPackage represents metadata from pom.xml files
type SbtPackage struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns npm for SbtPackage
func (m SbtPackage) Fetcher() string {
	// SBT uses the Maven repositories by default, but it looks like there's also
	// support for Ivy formats + "unmanaged dependencies" (vendored JARs?). TODO:
	// add support for these other formats.
	return "mvn"
}

// Package returns the package spec for SbtPackage
func (m SbtPackage) Package() string {
	return m.Name
}

// Revision returns the version spec for SbtPackage
func (m SbtPackage) Revision() string {
	return m.Version
}

// Initialize collects environment data for Bundler builds
func (ctx *SBTContext) Initialize(p *Module, opts map[string]interface{}) {
	ctx.SbtCmd = string(os.Getenv("SBT_BINARY"))
	if ctx.SbtCmd == "" {
		ctx.SbtCmd = "sbt"
	}
	outSbtVersion, err := exec.Command(ctx.SbtCmd, "-no-colors", "sbtVersion").Output()
	if err == nil {
		// TODO: support for multiple SBT versions
		sbtVersionLines := strings.Split(string(outSbtVersion), "\n")
		for i, versionLine := range sbtVersionLines {
			versionLine = strings.TrimSpace(versionLine)
			if strings.Index(versionLine, "sbtVersion") != -1 {
				ctx.SbtVersion = strings.TrimSpace(strings.TrimPrefix(sbtVersionLines[i+1], "[info] 	"))
			}
		}
	}

	if ctx.SbtVersion == "" {
		ctx.SbtCmd = ""
		ctx.SbtVersion = ""
	}
}

// Sets ctx.cachedSbtDepListOutput
func (ctx *SBTContext) getDepList(m *Module, opts map[string]interface{}) error {
	if ctx.cachedSbtDepListOutput != "" {
		return nil
	}
	cmdOut, err := exec.Command(ctx.SbtCmd, "-no-colors", "dependencyList").Output()
	if err != nil {
		ctx.cachedSbtDepListOutput = ""
		return err
	}

	ctx.cachedSbtDepListOutput = string(cmdOut)
	return nil
}

// Verify checks if the bundler is satisfied and if an install is necessary
func (ctx *SBTContext) Verify(p *Module, opts map[string]interface{}) bool {
	// TODO: test by running sbt dependencyTree and seeing if it fails
	return ctx.getDepList(p, opts) == nil && ctx.cachedSbtDepListOutput != ""
}

// Build runs Bundler and collect dep data
func (ctx *SBTContext) Build(m *Module, opts map[string]interface{}) error {
	if ctx.SbtCmd == "" || ctx.SbtVersion == "" {
		return errors.New("no SBT or jdk installation detected (try setting the $SBT_BINARY environment variable)")
	}

	// TODO: do not rely on the Verify step state to trigger build status
	if ctx.cachedSbtDepListOutput == "" {
		Logger.Debug("SBT project not built, running full install...")
		// bundle install, no flags as we need to satisfy all reqs
		exec.Command(ctx.SbtCmd, "clean", "compile").Output()
	}

	err := ctx.getDepList(m, opts)
	if err != nil || ctx.cachedSbtDepListOutput == "" {
		return errors.New("unable to extract SBT dependency list; have you built the artifact with `sbt compile` and installed `net.virtual-void` % `sbt-dependency-graph`?")
	}

	// process bundle list output
	dependencies := []Dependency{}
	for _, depLine := range strings.Split(string(ctx.cachedSbtDepListOutput), "\n") {
		if strings.Index(depLine, "[info] Loading ") != -1 ||
			strings.Index(depLine, "[info] Resolving ") != -1 ||
			strings.Index(depLine, "[info] Set ") != -1 ||
			strings.Index(depLine, "[info] Updating ") != -1 ||
			strings.Index(depLine, "[info] Done ") != -1 ||
			strings.Index(depLine, "[info] ") != 0 {
			continue
		}
		Logger.Debugf("filtered depLine: '%s'\n", depLine)

		if len(depLine) > 0 {
			depLocator := strings.TrimSpace(strings.TrimPrefix(depLine, "[info] "))
			depSections := strings.Split(depLocator, ":")
			dependencies = append(dependencies, Dependency(SbtPackage{
				Name:    depSections[0] + ":" + depSections[1],
				Version: depSections[2],
			}))
		}
	}

	m.Build.RawDependencies = Dedupe(dependencies)
	return nil
}
