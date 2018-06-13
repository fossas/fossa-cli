package gradle

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/builders/builderutil"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

// GradleBuilder implements Builder for build.gradle builds
type GradleBuilder struct {
	GradleCmd     string
	GradleVersion string
}

// Initialize collects metadata on Java and Maven binaries
func (builder *GradleBuilder) Initialize() error {
	log.Logger.Debug("Initializing Gradle builder...")

	// Set Gradle context variables
	gradleCmd, gradleVersionOut, err := exec.WhichArgs([]string{"--version", "--offline"}, os.Getenv("GRADLE_BINARY"), "./gradlew", "gradle")
	if err == nil {
		builder.GradleCmd = gradleCmd

		gradleVersionMatchRe := regexp.MustCompile(`Gradle ([0-9]+\.[0-9]+.\w+)`)
		match := gradleVersionMatchRe.FindStringSubmatch(gradleVersionOut)
		if len(match) == 2 {
			builder.GradleVersion = match[1]
		}
	}

	if builder.GradleCmd == "" {
		return fmt.Errorf("could not find Gradle binary or wrapper (try setting $GRADLE_BINARY)")
	}

	log.Logger.Debugf("Done initializing Gradle builder: %#v", builder)
	return nil
}

// Build is not implemented
func (builder *GradleBuilder) Build(m module.Module, force bool) error {
	return nil
}

// Analyze parses the output of `gradle -q app:dependencies`
func (builder *GradleBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Logger.Debugf("Running Gradle analysis: %#v %#v in %s", m, allowUnresolved, m.Dir)

	moduleConfigurationKey := strings.Split(m.Name, ":")

	taskName := "dependencies" // defaults to root gradle dependencies task

	if moduleConfigurationKey[0] != "" {
		// a subtask was configured
		taskName = moduleConfigurationKey[0] + ":dependencies"
	}

	taskConfiguration := "compile"
	if len(moduleConfigurationKey) == 2 && moduleConfigurationKey[1] != "" {
		taskConfiguration = moduleConfigurationKey[1]
	}

	// NOTE: we are intentionally using exec.Command over runLogged here, due to path issues with defining cmd.Dir
	dependenciesOutput, _, err := exec.Run(exec.Cmd{
		Name: builder.GradleCmd,
		Argv: []string{taskName, "-q", "--configuration=" + taskConfiguration, "--offline", "-a"},
		WithEnv: map[string]string{
			"TERM": "dumb",
		},
	})
	if len(dependenciesOutput) == 0 || err != nil {
		return nil, fmt.Errorf("could not run `gradle task %s:dependencies`", taskName)
	}

	var imports []builderutil.Imported
	root := module.Locator{
		Fetcher:  "root",
		Project:  "root",
		Revision: "root",
	}
	from := module.ImportPath{root}
	r := regexp.MustCompile("^([ `+\\\\|-]+)([^ `+\\\\|-].+)$")
	for _, line := range strings.Split(string(dependenciesOutput), "\n") {
		// Skip non-dependency lines
		if !r.MatchString(line) {
			continue
		}
		// Match for context
		matches := r.FindStringSubmatch(line)
		depth := len(matches[1])
		if depth%5 != 0 {
			// Sanity check
			log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line, matches)
		}
		// Parse locator
		dep := matches[2]
		withoutOmit := strings.TrimSuffix(dep, " (*)")
		resolvedParts := strings.Split(withoutOmit, " -> ")
		var project string
		var revision string
		if len(resolvedParts) == 2 {
			project = resolvedParts[0][:strings.LastIndex(resolvedParts[0], ":")]
			revision = resolvedParts[1]
		} else {
			splitVersionIndex := strings.LastIndex(resolvedParts[0], ":")
			project = resolvedParts[0][:splitVersionIndex]
			revision = resolvedParts[0][splitVersionIndex+1:]
		}
		locator := module.Locator{
			Fetcher:  "mvn",
			Project:  project,
			Revision: revision,
		}
		// Add to imports
		from = from[:depth/5]
		imports = append(imports, builderutil.Imported{
			Locator: locator,
			// This seems like a no-op, but this causes a memory copy that prevents
			// the most bullshit bug you have ever seen.
			From: append(module.ImportPath{}, from...),
		})
		from = append(from, locator)
	}
	deps := builderutil.ComputeImportPaths(imports)

	log.Logger.Debugf("Done running Gradle analysis: %#v", deps)
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

// DiscoverModules finds either a root build.gradle file in the specified dir
func (builder *GradleBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	if err := builder.Initialize(); err != nil {
		return nil, err
	}
	// Look for the root Gradle build
	// TODO: support custom *.gradle files
	if _, err := os.Stat(filepath.Join(dir, "build.gradle")); err == nil {
		// Use bare exec.Command as runLogged errors when resolving outside of dir
		taskListOutput, _, gradleTaskErr := exec.Run(exec.Cmd{
			Name: builder.GradleCmd,
			Argv: []string{"tasks", "--all", "-q", "-a", "--offline"},
		})
		if len(taskListOutput) > 0 && gradleTaskErr == nil {
			// Search for subprojects using Gradle task list instead of grepping for build.gradle
			var moduleConfigurations []module.Config
			// NOTE: this leaves out the root ("") dependencies task. To include, replace with `(\w+:)?dependencies -`
			// TODO: check for root dependencies task if not found otherwise
			taskListRe := regexp.MustCompile(`\w+:dependencies -`)
			for _, line := range strings.Split(string(taskListOutput), "\n") {
				trimmed := strings.TrimSpace(line)
				if len(trimmed) > 0 {
					depMatchIndices := taskListRe.FindStringIndex(trimmed)
					if len(depMatchIndices) > 0 && depMatchIndices[0] == 0 {
						gradleTask := strings.Split(trimmed, " - ")[0]
						log.Logger.Debugf("found gradle dependencies task: %s", gradleTask)
						moduleConfigurations = append(moduleConfigurations, module.Config{
							Name: strings.Split(gradleTask, ":")[0] + ":compile", // Name is the gradle `task:configuration` (default to compile)
							Path: "build.gradle",
							Type: "gradle",
						})
					}
				}
			}

			if len(moduleConfigurations) > 0 {
				return moduleConfigurations, nil
			}

			// If task list succeeds but returns no subproject dependencies tasks, fall back to the root `dependencies` task
			return []module.Config{
				{
					Name: ":compile",
					Path: "build.gradle",
					Type: "gradle",
				},
			}, nil
		}

		// If task list fails, fall back to "app" as default task, even though technically it would be "" (root)
		return []module.Config{
			{
				Name: "app:compile",
				Path: "build.gradle",
				Type: "gradle",
			},
		}, nil
	}

	// TODO: support multiple build.gradle files
	return nil, nil
}
