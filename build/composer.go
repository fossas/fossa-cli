package build

import (
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

// ComposerContext implements build context for Composer (composer.json) builds
type ComposerContext struct {
	ComposerCmd     string
	ComposerVersion string

	PhpCmd     string
	PhpVersion string

	isBuildSatisfied bool
}

// ComposerPackage represents metadata from Composer files
type ComposerPackage struct {
	Name    string `json:"name"`
	Version string `json:"version"`

	description string
}

// Fetcher always returns npm for ComposerPackage
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

// Initialize collects environment data for Bundler builds
func (ctx *ComposerContext) Initialize(p *Module, opts map[string]interface{}) {
	ctx.ComposerCmd = string(os.Getenv("COMPOSER_BINARY"))
	if ctx.ComposerCmd == "" {
		ctx.ComposerCmd = "composer"
	}
	outComposerVersion, err := exec.Command(ctx.ComposerCmd, "-V").Output()
	if err == nil {
		outputMatchRe := regexp.MustCompile(`Composer version ([0-9]+\.[0-9]+\.[0-9]+)`)
		match := outputMatchRe.FindStringSubmatch(strings.TrimSpace(string(outComposerVersion)))
		if len(match) == 2 {
			ctx.ComposerVersion = match[1]
		}
	} else {
		ctx.ComposerCmd = ""
		ctx.ComposerVersion = ""
	}

	ctx.PhpCmd = string(os.Getenv("PHP_BINARY"))
	if ctx.PhpCmd == "" {
		ctx.PhpCmd = "php"
	}
	outPhpVersion, err := exec.Command(ctx.PhpCmd, "-v").Output()
	if err == nil {
		outputPhpMatchRe := regexp.MustCompile(`PHP ([0-9]+\.[0-9]+\.[0-9]+)`)
		match := outputPhpMatchRe.FindStringSubmatch(strings.TrimSpace(string(outPhpVersion)))
		if len(match) == 2 {
			ctx.PhpVersion = match[1]
		}
	} else {
		ctx.PhpCmd = ""
		ctx.PhpVersion = ""
	}

	ctx.isBuildSatisfied = ctx.Verify(p, opts)
}

// Verify checks if the bundler is satisfied and if an install is necessary
func (ctx *ComposerContext) Verify(p *Module, opts map[string]interface{}) bool {
	// checks if composer.lock is satisfied by composer.json requirements
	// when fully installed, composer.lock will always be updated and satisfied
	// NOTE: we can't check the `vendor` dir as composer also relies on a global package dir that can satisfy reqs
	if _, err := exec.Command(ctx.ComposerCmd, "validate").Output(); err == nil {
		return true
	}
	return false
}

// Build runs Bundler and collect dep data
func (ctx *ComposerContext) Build(p *Module, opts map[string]interface{}) error {
	if ctx.PhpVersion == "" || ctx.PhpCmd == "" {
		return errors.New("no PHP installation detected; try setting the $PHP_BINARY environment variable")
	}

	if ctx.ComposerCmd == "" || ctx.ComposerVersion == "" {
		return errors.New("no Composer installation detected; try setting the $COMPOSER_BINARY environment variable")
	}

	if ctx.isBuildSatisfied == false {
		log.Log.Debug("composer build not satisfied, running full install")
		exec.Command(ctx.ComposerCmd, "install", "--prefer-dist", "--no-dev", "--no-autoloader", "--no-scripts", "--no-progress", "--no-suggest", "--ignore-platform-reqs").Output()
	}

	outBundleListCmd, err := exec.Command(ctx.ComposerCmd, "show", "-f", "json").Output()
	if err != nil {
		return errors.New("Unable to list composer dependencies")
	}

	// process composer json output
	composerOutData := map[string][]ComposerPackage{}
	dec := json.NewDecoder(strings.NewReader(string(outBundleListCmd)))

	if dec.Decode(&composerOutData) != nil {
		return errors.New("Unable to list parse composer dependency output: " + err.Error())
	}

	dedupedDependencies := []Dependency{}
	for _, d := range composerOutData["installed"] {
		dedupedDependencies = append(dedupedDependencies, Dependency(d))
	}

	p.Build.Dependencies = Dedupe(dedupedDependencies)
	return nil
}
