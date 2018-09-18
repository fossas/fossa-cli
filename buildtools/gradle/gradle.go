package gradle

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/pkg/errors"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
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

func (g *Gradle) Dependencies(project, configuration string) ([]Dependency, map[Dependency][]Dependency, error) {
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

func (g *Gradle) DependenciesTask(taskArgs ...string) ([]Dependency, map[Dependency][]Dependency, error) {
	stdout, err := g.Run(taskArgs...)
	if err != nil {
		return nil, nil, err
	}
	imports, deps, err := ParseDependencies(stdout)
	if err != nil {
		return nil, nil, err
	}
	return imports, deps, nil
}

// TODO: rename this -- this is really projects with :dependencies tasks
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
	log.Debugf("%#v", g)
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

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package gradle/' > readtree_generated.go"

func ParseDependencies(stdout string) ([]Dependency, map[Dependency][]Dependency, error) {
	// nolint:megacheck
	r := regexp.MustCompile("^([ `+\\\\|-]+)([^ `+\\\\|-].+)$")

	// Skip non-dependency lines.
	var filteredLines []string
	for _, line := range strings.Split(stdout, "\n") {
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
		if strings.HasPrefix(withoutAnnotations, "project :") {
			parsed = Dependency{
				Name:      strings.TrimPrefix(dep, "project :"),
				IsProject: true,
			}
		} else {
			sections := strings.Split(withoutAnnotations, ":")
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

		log.Debugf("%#v %#v", depth/5, parsed)
		return depth / 5, parsed, nil
	})
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
