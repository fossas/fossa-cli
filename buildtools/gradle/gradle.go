package gradle

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
)

type Gradle struct {
	Cmd    string
	Dir    string
	Online bool
}

type Dependency struct {
	Name      string
	Target    string
	Resolved  string
	IsProject bool
}

func (g *Gradle) Dependencies(project, configuration string) (map[Dependency][]Dependency, error) {
	args := []string{
		project + ":dependencies",
		"--quiet",
	}
	if configuration != "" {
		args = append(args, "--configuration="+configuration)
	}
	if !g.Online {
		args = append(args, "--offline")
	}

	return g.DependenciesTask(args...)
}

func (g *Gradle) DependenciesTask(taskArgs ...string) (map[Dependency][]Dependency, error) {
	stdout, err := g.Run(taskArgs...)
	if err != nil {
		return nil, err
	}
	deps, err := ParseDependencies(stdout)
	if err != nil {
		return nil, err
	}
	return deps, nil
}

func (g *Gradle) Projects() ([]string, error) {
	stdout, err := g.Run("tasks", "--all", "--quiet")
	if err != nil {
		return nil, err
	}
	var projects []string
	lines := strings.Split(stdout, "\n")
	for _, line := range lines {
		if i := strings.Index(line, ":dependencies"); i != -1 {
			projects = append(projects, line[:i])
		}
	}
	return projects, nil
}

func (g *Gradle) Run(taskArgs ...string) (string, error) {
	log.Logger.Debugf("%#v", g)
	stdout, _, err := exec.Run(exec.Cmd{
		Name: g.Cmd,
		Argv: taskArgs,
		Dir:  g.Dir,
	})
	if err != nil {
		return "", err
	}
	return stdout, nil
}

var Root = Dependency{}

func ParseDependencies(stdout string) (map[Dependency][]Dependency, error) {
	edges := make(map[Dependency]map[Dependency]bool)
	parents := []Dependency{Root}
	r := regexp.MustCompile("^([ `+\\\\|-]+)([^ `+\\\\|-].+)$")

	for _, line := range strings.Split(string(stdout), "\n") {
		// Skip non-dependency lines.
		if !r.MatchString(line) {
			continue
		}

		// Match line.
		matches := r.FindStringSubmatch(line)
		depth := len(matches[1])
		if depth%5 != 0 {
			// Sanity check.
			log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line, matches)
		}

		// Parse dependency.
		dep := matches[2]
		var parsed Dependency
		if strings.HasPrefix(dep, "project :") {
			parsed = Dependency{
				Name:      strings.TrimPrefix(dep, "project :"),
				IsProject: true,
			}
		} else {
			withoutOmit := strings.TrimSuffix(dep, " (*)")
			sections := strings.Split(withoutOmit, ":")
			name := strings.Join(sections[:len(sections)-1], ":")
			version := sections[len(sections)-1]
			target := version
			resolved := version
			versionSections := strings.Split(version, " -> ")
			if len(versionSections) == 2 {
				target = versionSections[0]
				resolved = versionSections[1]
			}
			parsed = Dependency{
				Name:     name,
				Target:   target,
				Resolved: resolved,
			}
		}

		// Add to graph.
		parents = parents[:depth/5]
		parent := parents[len(parents)-1]
		_, ok := edges[parent]
		if !ok {
			edges[parent] = make(map[Dependency]bool)
		}
		edges[parent][parsed] = true
		parents = append(parents, parsed)
	}

	graph := make(map[Dependency][]Dependency)
	for parent, children := range edges {
		for child := range children {
			graph[parent] = append(graph[parent], child)
		}
	}

	return graph, nil
}

func Cmd(dir string) (string, error) {
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
