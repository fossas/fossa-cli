package build

import (
	"errors"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

type BowerContext struct {
	BowerCmd     string
	BowerVersion string

	isBowerComponentsPrePopulated bool
}

// bower list . | tail +3 | grep -E "\w.+#\S+" -o | sed 's/^/bower\+/' | sed 's/#/$/' | sort | uniq

// Initialize collects environment data for CommonJS builds
func (ctx *BowerContext) Initialize(p *Module, opts map[string]interface{}) {
	// Set NodeJS context variables
	ctx.BowerCmd = string(os.Getenv("BOWER_BINARY"))
	if ctx.BowerCmd == "" {
		ctx.BowerCmd = "bower"
	}
	outBowerVersion, err := exec.Command(ctx.BowerCmd, "-v").Output()
	if err == nil && len(outBowerVersion) >= 5 {
		ctx.BowerVersion = strings.TrimSpace(string(outBowerVersion))
	} else {
		ctx.BowerCmd = ""
		ctx.BowerVersion = ""
	}

	ctx.isBowerComponentsPrePopulated = ctx.verifyBowerComponents()
}

// Verify checks if an install needs to be run
func (ctx *BowerContext) Verify(m *Module, opts map[string]interface{}) bool {
	return ctx.verifyBowerComponents()
}

// Build determines and executes a CommonJS build based off available tooling in the environment
func (ctx *BowerContext) Build(m *Module, opts map[string]interface{}) error {
	if ctx.BowerCmd == "" {
		return errors.New("no bower installation detected; try setting the $BOWER_BINARY environment variable")
	}

	// bower install
	if ctx.verifyBowerComponents() == false || opts["no_cache"].(bool) == true {
		log.Logger.Debug("No prebuilt bower_components directory, building...")
		exec.Command("bower", "install").Output()
	} else {
		log.Logger.Debug("Found pre-populated bower_components, skipping build...")
	}

	// verify again
	if ctx.verifyBowerComponents() == false {
		return errors.New("bower install did not satisfy build requirements")
	}

	outBundleListCmd, err := exec.Command("bower", "list").Output()
	if err != nil {
		return errors.New("unable to inspect bower components")
	}

	// process `bower list` output
	dependencies := []Dependency{}
	outputMatchRe := regexp.MustCompile("([a-z0-9_-]+)#([a-z0-9\\.]+)")
	for _, bundleListLn := range strings.Split(string(outBundleListCmd), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 {
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

func (ctx *BowerContext) verifyBowerComponents() bool {
	outBundleListCmd, err := exec.Command("bower", "list").Output()

	if err != nil {
		log.Logger.Warning("unable to verify Bower requirements... falling back to bower_components inspection")
		_, err := os.Stat("bower_components")
		return err == nil
	}

	// interrogate `bower list` to see if there's unsatisfied reqs
	// TODO: verify only production requirements
	outputMatchRe := regexp.MustCompile("not installed")
	for _, bundleListLn := range strings.Split(string(outBundleListCmd), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 {
			match := outputMatchRe.MatchString(bundleListLn)
			if match {
				return false
			}
		}
	}
	return true
}
