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

var mavenLogger = logging.MustGetLogger("maven")

// MavenArtifact implements Dependency for Maven builds
type MavenArtifact struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns mvn for MavenArtifact
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

// MavenBuilder implements Builder for Apache Maven (*.pom.xml) builds
type MavenBuilder struct {
	MvnCmd     string
	MvnVersion string

	JavaCmd     string
	JavaVersion string
}

func (builder *MavenBuilder) Initialize() error {
	builder.MvnCmd = string(os.Getenv("MVN_BINARY"))
	if builder.MvnCmd == "" {
		builder.MvnCmd = "mvn"
	}

	mavenVersionOutput, err := exec.Command(builder.MvnCmd, "-v").Output()
	if err != nil {
		return err
	}
	if len(mavenVersionOutput) >= 10 { // x.x.x
		outputMatchRe := regexp.MustCompile(`Apache Maven ([0-9]+\.[0-9]+\.[0-9]+)`)
		match := outputMatchRe.FindStringSubmatch(strings.TrimSpace(string(mavenVersionOutput)))
		if len(match) == 2 {
			builder.MvnVersion = match[1]
		}
	}

	if builder.MvnCmd == "" || builder.MvnVersion == "" {
		return errors.New("could not find mvn (try setting $MVN_BINARY)")
	}

	// TODO: collect Java information

	return nil
}

func (builder *MavenBuilder) Build(m module.Module, force bool) error {
	mavenLogger.Debugf("Running Maven build")
	if force {
		mavenLogger.Debugf("`force` flag is set: clearing install cache.")
		mavenLogger.Debugf("Running `mvn clean`...")
		cmd := exec.Command(builder.MvnCmd, "clean")
		cmd.Dir = m.Dir
		output, err := cmd.Output()
		mavenLogger.Debugf("...running `mvn clean` done: %#v", string(output))
		if err != nil {
			return err
		}
	}

	mavenLogger.Debugf("Running `mvn install -DskipTests -Drat.skip=true`...")
	cmd := exec.Command(builder.MvnCmd, "install", "-DskipTests", "-Drat.skip=true")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	mavenLogger.Debugf("...running `mvn install -DskipTests -Drat.skip=true` done: %#v", string(output))
	if err != nil {
		return err
	}

	return nil
}

func (builder *MavenBuilder) Analyze(m module.Module, _ bool) ([]module.Dependency, error) {
	mavenLogger.Debugf("Running Maven analysis")
	cmd := exec.Command(builder.MvnCmd, "dependency:list")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	deps := []module.Dependency{}
	outputMatchRe := regexp.MustCompile(`\[INFO\]    ([^:]+):([^:]+):(jar|war|java-source|):([^:]+)`)
	for _, bundleListLn := range strings.Split(string(output), "\n") {
		bundleListLn = strings.TrimSpace(bundleListLn)
		if len(bundleListLn) > 0 {
			match := outputMatchRe.FindStringSubmatch(bundleListLn)
			if len(match) == 5 {
				deps = append(deps, module.Dependency(MavenArtifact{
					Name:    match[1] + ":" + match[2],
					Version: match[4],
				}))
			}
		}
	}

	return deps, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *MavenBuilder) IsBuilt(m module.Module, _ bool) (bool, error) {
	mavenLogger.Debugf("Running `mvn dependency:list` for IsBuilt...")
	cmd := exec.Command(builder.MvnCmd, "dependency:list")
	cmd.Dir = m.Dir
	output, err := cmd.Output()
	outStr := string(output)
	mavenLogger.Debugf("...running `mvn dependency:list` done: %#v", outStr)
	if err != nil {
		if strings.Index(outStr, "Could not find artifact") != -1 {
			return false, nil
		}
		return false, err
	}
	return outStr != "", nil
}

func (builder *MavenBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for MavenBuilder")
}

func (builder *MavenBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for MavenBuilder")
}
