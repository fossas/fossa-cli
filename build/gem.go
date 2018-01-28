package build

import (
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

// GemContext implements build context for Bundler (Gemfile and *.gemspec) builds
type GemContext struct {
	GemCmd     string
	GemVersion string

	BundlerCmd     string
	BundlerVersion string
	// if the build is already satisfied we can skip install
	isBundlerSatisfied bool
}

// RubyGem represents metadata from Gemfiles
type RubyGem struct {
	Name     string `json:"name"`
	Version  string `json:"version"`
	resolved bool
}

// Fetcher always returns npm for RubyGem
func (m RubyGem) Fetcher() string {
	return "gem"
}

// Package returns the package spec for RubyGem
func (m RubyGem) Package() string {
	return m.Name
}

// Revision returns the version spec for RubyGem
func (m RubyGem) Revision() string {
	return m.Version
}

// Initialize collects environment data for Bundler builds
func (ctx *GemContext) Initialize(p *Module, opts map[string]interface{}) {
	ctx.GemCmd = string(os.Getenv("GEM_BINARY"))
	if ctx.GemCmd == "" {
		ctx.GemCmd = "gem"
	}
	outGemVersion, err := exec.Command(ctx.GemCmd, "-v").Output()
	if err == nil && len(outGemVersion) >= 5 { // x.x.x
		ctx.GemVersion = strings.TrimSpace(string(outGemVersion))
	} else {
		ctx.GemCmd = ""
		ctx.GemVersion = ""
	}

	ctx.BundlerCmd = string(os.Getenv("BUNDLER_BINARY"))
	if ctx.BundlerCmd == "" {
		ctx.BundlerCmd = "bundle"
	}
	outBundlerVersion, err := exec.Command(ctx.BundlerCmd, "-v").Output()
	if err == nil && strings.HasPrefix(string(outBundlerVersion), "Bundler version ") { // x.x.x
		ctx.BundlerVersion = strings.TrimSpace(string(outBundlerVersion))
	} else {
		ctx.BundlerCmd = ""
		ctx.BundlerVersion = ""
	}

	ctx.isBundlerSatisfied = ctx.Verify(p, opts)
}

// Verify checks if the bundler is satisfied and if an install is necessary
func (ctx *GemContext) Verify(m *Module, opts map[string]interface{}) bool {
	cmd := exec.Command(ctx.BundlerCmd, "check")
	cmd.Dir = m.Dir
	if _, err := cmd.Output(); err == nil {
		return true
	}
	return false
}

// Build runs Bundler and collect dep data
func (ctx *GemContext) Build(m *Module, opts map[string]interface{}) error {
	if ctx.BundlerCmd == "" || ctx.BundlerVersion == "" {
		return errors.New("no bundler installation detected -- falling back to gem; try setting the $BUNDLER_BINARY environment variable")
	}

	if ctx.isBundlerSatisfied == false {
		log.Logger.Debug("bundler not satisfied, running full install")
		// bundle install, no flags as we need to satisfy all reqs
		cmd := exec.Command(ctx.BundlerCmd, "install", "--deployment")
		cmd.Dir = m.Dir
		cmd.Output()
	}

	cmd := exec.Command(ctx.BundlerCmd, "list")
	cmd.Dir = m.Dir
	outBundleListCmd, err := cmd.Output()
	if err != nil {
		return errors.New("Unable to list rubygems")
	}

	// process bundle list output
	dependencies := []Dependency{}
	outputMatchRe := regexp.MustCompile("\\* ([a-z0-9_-]+) \\(([a-z0-9\\.]+)\\)")
	for _, bundleListLn := range strings.Split(string(outBundleListCmd), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 && bundleListLn[0] == '*' {
			match := outputMatchRe.FindStringSubmatch(bundleListLn)
			if len(match) == 3 {
				dependencies = append(dependencies, Dependency(RubyGem{
					Name:    match[1],
					Version: match[2],
				}))
			}
		}
	}

	m.Build.RawDependencies = Dedupe(dependencies)
	return nil
}
