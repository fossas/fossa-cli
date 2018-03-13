package builders

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
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

// Initialize collects metadata on Ruby, Gem, and Bundler binaries
func (builder *RubyBuilder) Initialize() error {
	rubyLogger.Debug("Initializing Ruby builder...")

	// Set Ruby context variables
	rubyCmd, rubyVersion, err := which("-v", os.Getenv("RUBY_BINARY"), "ruby")
	if err != nil {
		return fmt.Errorf("could not find Ruby binary (try setting $RUBY_BINARY): %s", err.Error())
	}
	builder.RubyCmd = rubyCmd
	builder.RubyVersion = rubyVersion

	// Set Gem context variables
	gemCmd, gemVersion, err := which("-v", os.Getenv("GEM_BINARY"), "gem")
	if err != nil {
		return fmt.Errorf("could not find Gem binary (try setting $GEM_BINARY): %s", err.Error())
	}
	builder.GemCmd = gemCmd
	builder.GemVersion = gemVersion

	// Set Bundler context variables
	bundlerCmd, bundlerVersion, err := which("-v", os.Getenv("BUNDLER_BINARY"), "bundler", "bundle")
	if err != nil {
		return fmt.Errorf("could not find Bundler binary (try setting $BUNDLER_BINARY): %s", err.Error())
	}
	builder.BundlerCmd = bundlerCmd
	builder.BundlerVersion = bundlerVersion

	rubyLogger.Debugf("Initialized Ruby builder: %#v", builder)
	return nil
}

// Build runs `bundler install --deployment --frozen` and cleans with `rm Gemfile.lock`
func (builder *RubyBuilder) Build(m module.Module, force bool) error {
	rubyLogger.Debugf("Running Ruby build: %#v %#v", m, force)

	if force {
		_, _, err := runLogged(rubyLogger, m.Dir, "rm", "Gemfile.lock")
		if err != nil {
			return fmt.Errorf("could not remove Ruby cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(rubyLogger, m.Dir, builder.BundlerCmd, "install", "--deployment", "--frozen")
	if err != nil {
		return fmt.Errorf("could not run Ruby build: %s", err.Error())
	}

	bowerLogger.Debug("Done running Ruby build.")
	return nil
}

// Analyze parses the output of `bundler list`
func (builder *RubyBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	rubyLogger.Debugf("Running Ruby analysis: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(rubyLogger, m.Dir, builder.BundlerCmd, "list")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from Bundler: %s", err.Error())
	}

	deps := []module.Dependency{}
	outputMatchRe := regexp.MustCompile("\\* ([a-z0-9_-]+) \\(([a-z0-9\\.]+)\\)")
	for _, line := range strings.Split(output, "\n") {
		trimmed := strings.TrimSpace(line)
		if len(trimmed) > 0 && trimmed[0] == '*' {
			match := outputMatchRe.FindStringSubmatch(trimmed)
			if len(match) == 3 {
				deps = append(deps, module.Dependency(RubyGem{
					Name:    match[1],
					Version: match[2],
				}))
			}
		}
	}

	rubyLogger.Debugf("Done running Ruby analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether `bundler check` exits with an error
func (builder *RubyBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	rubyLogger.Debugf("Checking Ruby build: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(rubyLogger, m.Dir, builder.BundlerCmd, "check")
	if err != nil {
		if strings.Index(output, "`bundle install`") != -1 {
			return false, nil
		}
		return false, err
	}

	rubyLogger.Debugf("Done checking Ruby build: %#v", true)
	return true, nil
}

// IsModule is not implemented
func (builder *RubyBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for RubyBuilder")
}

// DiscoverModules returns ModuleConfigs that match Gemfiles in the directory
func (builder *RubyBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	gemFilePaths, err := doublestar.Glob(filepath.Join(dir, "**", "Gemfile"))
	if err != nil {
		return nil, err
	}
	moduleConfigs := make([]module.Config, len(gemFilePaths))
	for i, path := range gemFilePaths {
		gemName := filepath.Base(filepath.Dir(path))
		// infer title from *.gemspec in directory if exists
		gemSpecs, err := doublestar.Glob(filepath.Join(filepath.Dir(path), "*.gemspec"))
		if err == nil && len(gemSpecs) > 0 {
			matchGemName := regexp.MustCompile(`\s+s\.name\s+=\s?"(\w+)"`)
			gemSpecContents, err := ioutil.ReadFile(gemSpecs[0])
			if err == nil {
				matches := matchGemName.FindStringSubmatch(string(gemSpecContents))
				// matches: [0] = full match, [1] capture group
				if len(matches) == 2 {
					rubyLogger.Debugf("%v", matches[1])
					gemName = matches[1]
				}
			}
		}
		path, _ = filepath.Rel(dir, path)
		moduleConfigs[i] = module.Config{
			Name: gemName,
			Path: path,
			Type: "ruby",
		}
	}
	return moduleConfigs, nil
}
