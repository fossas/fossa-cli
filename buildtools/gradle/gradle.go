package gradle

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// ShellCommand controls the information needed to run a gradle command.
type ShellCommand struct {
	Binary  string
	Dir     string
	Online  bool
	Timeout string
	Retries int
	Cmd     func(command string, timeout string, retries int, arguments ...string) (string, error)
}

// Dependency models a gradle dependency.
type Dependency struct {
	Name             string
	RequestedVersion string
	ResolvedVersion  string
	IsProject        bool
}

// Input is an interface for any point where gradle analysis requires input from
// a file, shell command, or other source.
type Input interface {
	ProjectDependencies(...string) (map[string]graph.Deps, error)
	DependencyTasks() ([]string, error)
}

// NewShellInput creates a new ShellCommand and returns it as an Input.
func NewShellInput(binary, dir string, online bool, timeout string, retries int) Input {
	return ShellCommand{
		Binary:  binary,
		Dir:     dir,
		Online:  online,
		Timeout: timeout,
		Retries: retries,
		Cmd:     Cmd,
	}
}

// MergeProjectsDependencies creates a complete configuration to dep graph map by
// looping through a given list of projects and merging their dependencies by configuration.
func MergeProjectsDependencies(i Input, projects []string) (map[string]graph.Deps, error) {
	configurationMap := make(map[string]graph.Deps)
	for _, project := range projects {
		depGraph, err := Dependencies(project, i)
		if err != nil {
			return configurationMap, err
		}
		for configuration, configGraph := range depGraph {
			if configurationMap[configuration].Direct == nil {
				configurationMap[configuration] = configGraph
			} else {
				for id, transitivePackage := range configGraph.Transitive {
					configurationMap[configuration].Transitive[id] = transitivePackage
				}
				tempDirect := configurationMap[configuration].Direct
				for _, dep := range configGraph.Direct {
					if !contains(tempDirect, dep) {
						tempDirect = append(tempDirect, dep)
					}
				}
				configurationMap[configuration] = graph.Deps{
					Direct:     tempDirect,
					Transitive: configurationMap[configuration].Transitive,
				}
			}
		}
	}

	return configurationMap, nil
}

// Dependencies returns the dependencies of a gradle project
func Dependencies(project string, i Input) (map[string]graph.Deps, error) {
	args := []string{
		project + ":dependencies",
		"--quiet",
	}

	return i.ProjectDependencies(args...)
}

// ProjectDependencies returns the dependencies of a given gradle project
func (s ShellCommand) ProjectDependencies(taskArgs ...string) (map[string]graph.Deps, error) {
	arguments := taskArgs
	if s.Dir != "" {
		arguments = append(arguments, "-p", s.Dir)
	}
	if !s.Online {
		arguments = append(arguments, "--offline")
	}
	stdout, err := s.Cmd(s.Binary, s.Timeout, s.Retries, arguments...)
	if err != nil {
		return nil, err
	}

	// Parse individual configurations.
	configurations := make(map[string]graph.Deps)
	// Divide output into configurations. Each configuration is separated by an
	// empty line, started with a configuration name (and optional description),
	// and has a dependency as its second line.
	splitReg := regexp.MustCompile("\r?\n\r?\n")
	sections := splitReg.Split(stdout, -1)
	for _, section := range sections {
		lines := strings.Split(section, "\n")
		if len(lines) < 2 {
			continue
		}
		config := strings.Split(lines[0], " - ")[0]
		if lines[1] == "No dependencies" {
			configurations[config] = graph.Deps{}
		} else if strings.HasPrefix(lines[1], "\\--- ") || strings.HasPrefix(lines[1], "+--- ") {
			imports, deps, err := ParseDependencies(section)
			if err != nil {
				return nil, err
			}
			configurations[config] = NormalizeDependencies(imports, deps)
		}
	}

	return configurations, nil
}

// DependencyTasks returns a list of gradle projects by analyzing a list of gradle tasks.
func (s ShellCommand) DependencyTasks() ([]string, error) {
	arguments := []string{"tasks", "--all", "--quiet"}
	if s.Dir != "" {
		arguments = append(arguments, "-p", s.Dir)
	}
	stdout, err := s.Cmd(s.Binary, s.Timeout, s.Retries, arguments...)
	if err != nil {
		log.Warnf("Error found running `%s %s`: %s", s.Binary, arguments, err)
		return nil, err
	}
	var projects []string
	lines := strings.Split(stdout, "\n")
	for _, line := range lines {
		if i := strings.Index(line, ":dependencies -"); i != -1 {
			projects = append(projects, line[:i])
		}
	}
	return projects, nil
}

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package gradle/' > readtree_generated.go"

func ParseDependencies(stdout string) ([]Dependency, map[Dependency][]Dependency, error) {
	r := regexp.MustCompile("^([ `+\\\\|-]+)([^ `+\\\\|-].+)$")
	splitReg := regexp.MustCompile("\r?\n")
	// Skip non-dependency lines.
	var filteredLines []string
	for _, line := range splitReg.Split(stdout, -1) {
		if r.MatchString(line) {
			filteredLines = append(filteredLines, line)
		}
	}

	return ReadDependencyTree(filteredLines, func(line string) (int, Dependency, error) {
		// Match line.
		matches := r.FindStringSubmatch(line)
		depth := len(matches[1])
		if depth%5 != 0 {
			// Sanity check.
			return -1, Dependency{}, errors.Errorf("bad depth: %#v %s %#v", depth, line, matches)
		}

		// Parse dependency.
		dep := matches[2]
		withoutAnnotations := strings.TrimSuffix(strings.TrimSuffix(strings.TrimSuffix(dep, " (*)"), " (n)"), " FAILED")
		var parsed Dependency
		if strings.HasPrefix(withoutAnnotations, "project ") {
			// TODO: the desired method for handling this might be to recurse into the subproject.
			parsed = Dependency{
				// The project name may or may not have a leading colon.
				Name:      strings.TrimPrefix(strings.TrimPrefix(withoutAnnotations, "project "), ":"),
				IsProject: true,
			}
		} else {
			// The line withoutAnnotations can have the form:
			//  1. group:project:requestedVersion
			//  2. group:project:requestedVersion -> resolvedVersion
			//  3. group:project -> resolvedVersion

			var name, requestedVer, resolvedVer string

			sections := strings.Split(withoutAnnotations, " -> ")
			requestedIsNotResolved := len(sections) == 2

			idSections := strings.Split(sections[0], ":")
			name = idSections[0]
			if len(idSections) > 1 {
				name += ":" + idSections[1]
				if len(idSections) > 2 {
					requestedVer = idSections[2]
				}
			}

			if requestedIsNotResolved {
				resolvedVer = sections[1]
			} else {
				resolvedVer = requestedVer
			}
			parsed = Dependency{
				Name:             name,
				RequestedVersion: requestedVer,
				ResolvedVersion:  resolvedVer,
			}
		}

		log.Debugf("%#v %#v", depth/5, parsed)
		return depth / 5, parsed, nil
	})
}

// Cmd executes the gradle shell command.
func Cmd(command string, timeout string, retries int, taskArgs ...string) (string, error) {
	tempcmd := exec.Cmd{
		Name:    command,
		Argv:    taskArgs,
		Timeout: timeout,
		Retries: retries,
	}

	stdout, stderr, err := exec.Run(tempcmd)
	if stderr != "" {
		return stdout, errors.Errorf("%s", stderr)
	}

	return stdout, err
}

// ValidBinary finds the best possible gradle command to run for
// shell commands.
func ValidBinary(dir string) (string, error) {
	gradleEnv := os.Getenv("FOSSA_GRADLE_CMD")
	if gradleEnv != "" {
		return gradleEnv, nil
	}

	ok, err := files.Exists(dir, "gradlew")
	if err != nil {
		return "", err
	}
	if ok {
		return filepath.Abs(filepath.Join(dir, "gradlew"))
	}

	ok, err = files.Exists(dir, "gradlew.bat")
	if err != nil {
		return "", err
	}
	if ok {
		return filepath.Abs(filepath.Join(dir, "gradlew.bat"))
	}

	return "gradle", nil
}

// NormalizeDependencies turns a dependency map into a FOSSA recognized dependency graph.
func NormalizeDependencies(imports []Dependency, deps map[Dependency][]Dependency) graph.Deps {
	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Target: dep.RequestedVersion,
			Resolved: pkg.ID{
				Type:     pkg.Gradle,
				Name:     dep.Name,
				Revision: dep.ResolvedVersion,
			},
		})
	}

	// Set transitive dependencies.
	d := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		id := pkg.ID{
			Type:     pkg.Gradle,
			Name:     parent.Name,
			Revision: parent.ResolvedVersion,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.ResolvedVersion,
				Resolved: pkg.ID{
					Type:     pkg.Gradle,
					Name:     child.Name,
					Revision: child.ResolvedVersion,
				},
			})
		}
		d[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	return graph.Deps{
		Direct:     i,
		Transitive: d,
	}
}

func contains(array []pkg.Import, check pkg.Import) bool {
	for _, val := range array {
		if val == check {
			return true
		}
	}
	return false
}
