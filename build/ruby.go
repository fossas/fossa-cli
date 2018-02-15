package build

import (
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/module"
	logging "github.com/op/go-logging"
)

var rubyLogger = logging.MustGetLogger("ruby")

// RubyGem implements Dependency for Bundler and RubyGems.
type RubyGem struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns gem for RubyGem
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

// RubyBuilder implements Builder for Bundler (Gemfile) builds
type RubyBuilder struct {
	BundlerCmd     string
	BundlerVersion string

	GemCmd     string
	GemVersion string

	RubyCmd     string
	RubyVersion string
}

// Initialize collects environment data for Bundler builds
func (builder *RubyBuilder) Initialize() error {
	builder.BundlerCmd = string(os.Getenv("BUNDLER_BINARY"))
	if builder.BundlerCmd == "" {
		builder.BundlerCmd = "bundle"
	}
	outBundlerVersion, err := exec.Command(builder.BundlerCmd, "-v").Output()
	if err != nil {
		return err
	}
	if strings.HasPrefix(string(outBundlerVersion), "Bundler version ") {
		builder.BundlerVersion = strings.TrimSpace(string(outBundlerVersion))
	}

	builder.GemCmd = string(os.Getenv("GEM_BINARY"))
	if builder.GemCmd == "" {
		builder.GemCmd = "gem"
	}
	outGemVersion, err := exec.Command(builder.GemCmd, "-v").Output()
	if err != nil {
		return err
	}
	if len(outGemVersion) >= 5 { // x.x.x
		builder.GemVersion = strings.TrimSpace(string(outGemVersion))
	}

	// TODO: collect Ruby command information

	if builder.BundlerCmd == "" || builder.BundlerVersion == "" {
		return errors.New("could not find bundler (try setting $BUNDLER_BINARY)")
	}

	rubyLogger.Debugf("Initialized Ruby builder: %#v", builder)
	return nil
}

// Build runs Bundler and collect dep data
func (builder *RubyBuilder) Build(m module.Module, force bool) error {
	rubyLogger.Debugf("Running Ruby build...")
	if force {
		cmd := exec.Command("rm", "Gemfile.lock")
		cmd.Dir = m.Dir
		if _, err := cmd.Output(); err != nil {
			return err
		}
	}
	cmd := exec.Command(builder.BundlerCmd, "install", "--deployment", "--frozen")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	if err != nil {
		rubyLogger.Debugf("Ruby build failed: %#v", string(output))
		return err
	}
	return nil
}

func (builder *RubyBuilder) Analyze(m module.Module, _ bool) ([]module.Dependency, error) {
	cmd := exec.Command(builder.BundlerCmd, "list")
	cmd.Dir = m.Dir
	outBundleListCmd, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	// process bundle list output
	deps := []module.Dependency{}
	outputMatchRe := regexp.MustCompile("\\* ([a-z0-9_-]+) \\(([a-z0-9\\.]+)\\)")
	for _, bundleListLn := range strings.Split(string(outBundleListCmd), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 && bundleListLn[0] == '*' {
			match := outputMatchRe.FindStringSubmatch(bundleListLn)
			if len(match) == 3 {
				deps = append(deps, module.Dependency(RubyGem{
					Name:    match[1],
					Version: match[2],
				}))
			}
		}
	}

	return deps, nil
}

// Verify checks if the bundler is satisfied and if an install is necessary
func (builder *RubyBuilder) IsBuilt(m module.Module, _ bool) (bool, error) {
	cmd := exec.Command(builder.BundlerCmd, "check")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	outStr := string(output)
	if err != nil {
		if strings.Index(outStr, "Please run `bundle install`") != -1 {
			return false, nil
		}
		return false, err
	}
	return true, nil
}

func (builder *RubyBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for RubyBuilder")
}

func (builder *RubyBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for RubyBuilder")
}
