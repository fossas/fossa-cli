package builders

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
)

var gradleLogger = logging.MustGetLogger("gradle")

// GradleBuilder implements Builder for build.gradle builds
type GradleBuilder struct {
	JavaCmd     string
	JavaVersion string

	GradleCmd     string
	GradleVersion string
}

// Initialize collects metadata on Java and Maven binaries
func (builder *GradleBuilder) Initialize() error {
	gradleLogger.Debug("Initializing Maven builder...")

	// Set Java context variables
	javaCmd, javaVersion, err := which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		return fmt.Errorf("could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}
	builder.JavaCmd = javaCmd
	builder.JavaVersion = javaVersion

	// Set Maven context variables
	gradleCmd, gradleVersion, err := which("--version", "gradlew", os.Getenv("GRADLE_BINARY"), "gradle")
	if err != nil {
		return fmt.Errorf("could not find Gradle binary or wrapper (try setting $GRADLE_BINARY): %s", err.Error())
	}
	builder.GradleCmd = gradleCmd
	builder.GradleVersion = gradleVersion

	gradleLogger.Debugf("Done initializing Gradle builder: %#v", builder)
	return nil
}

// Build is not implemented
func (builder *GradleBuilder) Build(m module.Module, force bool) error {
	return nil
}

// Analyze parses the output of `gradle -q app:dependencies`
func (builder *GradleBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	// TODO: allow configuration of root
	gradleLogger.Debugf("Running Maven analysis: %#v %#v", m, allowUnresolved)

	output, _, err := runLogged(gradleLogger, m.Dir, builder.GradleCmd, "-q", "app:dependencies")
	if err != nil {
		return nil, fmt.Errorf("could not get dependency list from Maven: %s", err.Error())
	}

	deps := []module.Dependency{}
	outputMatchRe := regexp.MustCompile(`\[INFO\]    ([^:]+):([^:]+):(jar|war|java-source|):([^:]+)`)
	for _, line := range strings.Split(output, "\n") {
		trimmed := strings.TrimSpace(line)
		if len(trimmed) > 0 {
			match := outputMatchRe.FindStringSubmatch(trimmed)
			if len(match) == 5 {
				deps = append(deps, module.Dependency(MavenArtifact{
					Name:    match[1] + ":" + match[2],
					Version: match[4],
				}))
			}
		}
	}

	gradleLogger.Debugf("Done running Gradle analysis: %#v", deps)
	return deps, nil
}

// IsBuilt is not implemented
func (builder *GradleBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	return true, nil
}

// IsModule is not implemented
func (builder *GradleBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for GradleBuilder")
}

// DiscoverModules finds either a root pom.xml file or all pom.xmls in the specified dir
func (builder *GradleBuilder) DiscoverModules(dir string) ([]config.ModuleConfig, error) {
	gradleLogger.Debug("asd")
	_, err := os.Stat(filepath.Join(dir, "build.gradle")) // TODO: support custom *.gradle files
	if err == nil {
		// Root gradle build found; parse and return for a better UX with multi-project builds
		absDir, err := filepath.Abs(dir)
		if err != nil {
			absDir = dir
		}
		artifactName := filepath.Base(absDir)
		return []config.ModuleConfig{
			config.ModuleConfig{
				Name: artifactName,
				Path: "build.gradle",
				Type: "gradle",
			},
		}, nil
	}

	// TODO: support multiple build.gradle files
	return []config.ModuleConfig{}, nil
}
