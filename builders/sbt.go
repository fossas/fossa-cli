package builders

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
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

// Initialize collects metadata on Java and SBT binaries
func (builder *SBTBuilder) Initialize() error {
	sbtLogger.Debugf("Initializing SBT builder...")

	// Set Java context variables
	javaCmd, javaVersion, err := which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		return fmt.Errorf("could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}
	builder.JavaCmd = javaCmd
	builder.JavaVersion = javaVersion

	// Set SBT context variables
	sbtCmd, sbtVersion, err := which("-no-colors about", os.Getenv("SBT_BINARY"), "sbt")
	if err != nil {
		return fmt.Errorf("could not find SBT binary (try setting $SBT_BINARY): %s", err.Error())
	}
	builder.SBTCmd = sbtCmd
	builder.SBTVersion = sbtVersion

	sbtLogger.Debugf("Initialized SBT builder: %#v", builder)
	return nil
}

// Build runs `sbt compile` and cleans with `sbt clean`
func (builder *SBTBuilder) Build(m module.Module, force bool) error {
	sbtLogger.Debugf("Running SBT build: %#v %#v", m, force)

	if force {
		_, _, err := runLogged(sbtLogger, m.Dir, builder.SBTCmd, "clean")
		if err != nil {
			return fmt.Errorf("could not remove SBT cache: %s", err.Error())
		}
	}

	_, _, err := runLogged(sbtLogger, m.Dir, builder.SBTCmd, "compile")
	if err != nil {
		return fmt.Errorf("could not run SBT build: %s", err.Error())
	}

	sbtLogger.Debug("Done running SBT build.")
	return nil
}

// Analyze parses the output of `sbt -no-colors dependencyList`
func (builder *SBTBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	sbtLogger.Debugf("Running SBT analysis: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(sbtLogger, m.Dir, builder.SBTCmd, "-no-colors", "dependencyList")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from SBT: %s", err.Error())
	}

	deps := []module.Dependency{}
	for _, line := range strings.Split(output, "\n") {
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

	sbtLogger.Debugf("Done running SBT analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *SBTBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	sbtLogger.Debugf("Checking SBT build: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(sbtLogger, m.Dir, builder.SBTCmd, "-no-colors", "dependencyList")
	if err != nil {
		return false, fmt.Errorf("could not get dependency list from SBT: %s", err.Error())
	}
	isBuilt := output != ""

	sbtLogger.Debugf("Done checking SBT build: %#v", isBuilt)
	return isBuilt, nil
}

// IsModule is not implemented
func (builder *SBTBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for SBTBuilder")
}

// DiscoverModules returns a root build.sbt if found, and build configs for all sub-projects otherwise
func (builder *SBTBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	_, err := os.Stat(filepath.Join(dir, "build.sbt"))
	if err == nil {
		// in a multi-project build (https://www.scala-sbt.org/1.x-beta/docs/Multi-Project.html) we return the root build.sbt only
		absDir, err := filepath.Abs(dir)
		if err != nil {
			absDir = dir
		}
		artifactName := filepath.Base(absDir)
		return []module.Config{
			module.Config{
				Name: artifactName,
				Path: "build.sbt",
				Type: "sbt",
			},
		}, nil
	}

	// no pom in root directory; find and parse all of them
	sbtFilePaths, err := doublestar.Glob(filepath.Join(dir, "**", "build.sbt"))
	if err != nil {
		return nil, err
	}
	moduleConfigs := make([]module.Config, len(sbtFilePaths))
	for i, path := range sbtFilePaths {
		artifactName := filepath.Dir(path) // Use the dirname as it's impossible to reliably parse from build.sbt
		moduleConfigs[i] = module.Config{
			Name: artifactName,
			Path: path,
			Type: "sbt",
		}
	}
	return moduleConfigs, nil
}
