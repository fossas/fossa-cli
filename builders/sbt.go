package builders

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var sbtLogger = logging.MustGetLogger("sbt")

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
		sbtLogger.Warningf("Could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
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

// Analyze parses the output of `sbt -no-colors dependencyTree`
func (builder *SBTBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	sbtLogger.Debugf("Running SBT analysis: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(sbtLogger, m.Dir, builder.SBTCmd, "-no-colors", "dependencyTree")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency tree from SBT: %s", err.Error())
	}

	// Filter lines to only include dependency tree
	var depLines []string
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
		depLines = append(depLines, line)
	}

	// Parse dependency tree
	var imports []Imported
	root := module.Locator{
		Fetcher:  "root",
		Project:  "root",
		Revision: "root",
	}
	from := module.ImportPath{root}
	depRegex := regexp.MustCompile("^\\[info\\] ([ `+\\\\|-]*)([^ `+\\\\|-].+)$")
	spacerRegex := regexp.MustCompile("^\\[info\\] ([ `+\\\\|-]*)(\\s*?)$")
	locatorRegex := regexp.MustCompile("([^:\\s]+):([^:\\s]+):([^:\\s]+).*")
	for _, line := range depLines {
		sbtLogger.Debugf("Parsing line: %#v\n", line)
		if spacerRegex.MatchString(line) {
			continue
		}
		// Match for context
		depMatches := depRegex.FindStringSubmatch(line)
		sbtLogger.Debugf("Dep matches: %#v\n", depMatches)
		depth := len(depMatches[1])
		// SBT quirk: the indentation from level 1 to level 2 is 4 spaces, but all others are 2 spaces
		if depth >= 4 {
			depth -= 2
		}
		if depth%2 != 0 {
			// Sanity check
			sbtLogger.Panicf("Bad depth: %#v %s %#v", depth, line, depMatches)
		}
		// Parse locator
		locatorMatches := locatorRegex.FindStringSubmatch(depMatches[2])
		sbtLogger.Debugf("Locator matches: %#v\n", locatorMatches)
		locator := module.Locator{
			Fetcher:  "sbt",
			Project:  locatorMatches[1] + ":" + locatorMatches[2],
			Revision: locatorMatches[3],
		}
		// Add to imports
		from = from[:depth/2]
		imports = append(imports, Imported{
			Locator: locator,
			From:    append(module.ImportPath{}, from...),
		})
		from = append(from, locator)
	}
	deps := computeImportPaths(imports)

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
			{
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
