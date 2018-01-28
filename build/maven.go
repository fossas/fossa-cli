package build

import (
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	. "github.com/fossas/fossa-cli/log"
)

// MavenContext implements build context for Apache Maven (*pom.xml) builds
type MavenContext struct {
	MvnCmd     string
	MvnVersion string

	JavaCmd     string
	JavaVersion string

	// since we rely on dependency:list to verify the build, we will its initialization for now to also determine if the build is satisfied
	// TODO: change this behavior
	cachedMvnDepListOutput string
}

// MavenArtifact represents metadata from pom.xml files
type MavenArtifact struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns npm for MavenArtifact
func (m MavenArtifact) Fetcher() string {
	return "mvn"
}

// Package returns the package spec for MavenArtifact
func (m MavenArtifact) Package() string {
	return m.Name
}

// Revision returns the version spec for MavenArtifact
func (m MavenArtifact) Revision() string {
	return m.Version
}

// Initialize collects environment data for Bundler builds
func (ctx *MavenContext) Initialize(p *Module, opts map[string]interface{}) {
	ctx.MvnCmd = string(os.Getenv("MVN_BINARY"))
	if ctx.MvnCmd == "" {
		ctx.MvnCmd = "mvn"
	}
	outMvnVersion, err := exec.Command(ctx.MvnCmd, "-v").Output()
	if err == nil && len(outMvnVersion) >= 10 { // x.x.x
		outputMatchRe := regexp.MustCompile(`Apache Maven ([0-9]+\.[0-9]+\.[0-9]+)`)
		match := outputMatchRe.FindStringSubmatch(strings.TrimSpace(string(outMvnVersion)))
		if len(match) == 2 {
			ctx.MvnVersion = match[1]
		}
	}

	if ctx.MvnVersion == "" {
		ctx.MvnCmd = ""
		ctx.MvnVersion = ""
	}
}

// Sets ctx.cachedMvnDepListOutput
func (ctx *MavenContext) getDepList(m *Module, opts map[string]interface{}) error {
	if ctx.cachedMvnDepListOutput != "" {
		return nil
	}
	cmdOut, err := exec.Command(ctx.MvnCmd, "dependency:list", "-f", m.Manifest).Output()
	ctx.cachedMvnDepListOutput = string(cmdOut)
	return err
}

// Verify checks if the bundler is satisfied and if an install is necessary
func (ctx *MavenContext) Verify(p *Module, opts map[string]interface{}) bool {
	// TODO: test by running mvn dependency:list and seeing if it fails
	return ctx.getDepList(p, opts) == nil && ctx.cachedMvnDepListOutput != ""
}

// Build runs Bundler and collect dep data
func (ctx *MavenContext) Build(m *Module, opts map[string]interface{}) error {
	if ctx.MvnCmd == "" || ctx.MvnVersion == "" {
		return errors.New("no maven or jdk installation detected -- try setting the $MVN_BINARY environment variable.")
	}

	// TODO: do not rely on the Verify step state to trigger build status
	if ctx.cachedMvnDepListOutput == "" {
		Logger.Debug("maven project not built, running full install...")
		// bundle install, no flags as we need to satisfy all reqs
		exec.Command(ctx.MvnCmd, "clean", "install", "-DskipTests", "-Drat.skip=true", "-f", m.Manifest).Output()
	}

	err := ctx.getDepList(m, opts)
	if err != nil || ctx.cachedMvnDepListOutput == "" {
		return errors.New("unable to extract maven dependency list; have you built the artifact with `mvn install` yet?")
	}

	// process bundle list output
	dependencies := []Dependency{}
	outputMatchRe := regexp.MustCompile(`\[INFO\]    ([^:]+):([^:]+):(jar|war|java-source|):([^:]+)`)
	for _, bundleListLn := range strings.Split(string(ctx.cachedMvnDepListOutput), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 {
			match := outputMatchRe.FindStringSubmatch(bundleListLn)
			if len(match) == 5 {
				dependencies = append(dependencies, Dependency(MavenArtifact{
					Name:    match[1] + ":" + match[2],
					Version: match[4],
				}))
			}
		}
	}

	m.Build.RawDependencies = Dedupe(dependencies)
	return nil
}
