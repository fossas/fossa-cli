package maven

import (
	"encoding/xml"
	"regexp"
	"strings"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
)

type Manifest struct {
	Project     xml.Name
	ArtifactID  string
	GroupID     string
	Version     string
	Description string
	Name        string
	URL         string
}

type Dependency struct {
	Name    string
	Version string
	Failed  bool
}

type Maven struct {
	Cmd string
}

func (m *Maven) Clean(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: m.Cmd,
		Argv: []string{"clean", "--batch-mode"},
		Dir:  dir,
	})
	return err
}

func (m *Maven) Compile(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: m.Cmd,
		Argv: []string{"compile", "-DskipTests", "-Drat.skip=true", "--batch-mode"},
		Dir:  dir,
	})
	return err
}

func (m *Maven) Modules(dir string) ([]string, error) {
	deps, err := m.DependencyList(dir)
	if err != nil {
		return nil, err
	}
	var modules []string
	lines := strings.Split(deps, "\n")
	r := regexp.MustCompile("^\\[INFO\\] --+< (.*?) >--+$")
	for _, line := range lines {
		if r.MatchString(line) {
			matches := r.FindStringSubmatch(line)
			modules = append(modules, matches[1])
		}
	}
	return modules, nil
}

func (m *Maven) DependencyList(dir string) (string, error) {
	output, _, err := exec.Run(exec.Cmd{
		Name: m.Cmd,
		Dir:  dir,
		Argv: []string{"dependency:list", "--batch-mode"},
	})
	return output, err
}

func (m *Maven) DependencyTree(dir, project string) ([]Dependency, map[Dependency][]Dependency, error) {
	output, _, err := exec.Run(exec.Cmd{
		Name: m.Cmd,
		Dir:  dir,
		Argv: []string{"dependency:tree", "--batch-mode", "--projects", project},
	})
	if err != nil {
		return nil, nil, err
	}

	return ParseDependencyTree(output)
}

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package maven/' > readtree_generated.go"

func ParseDependencyTree(stdin string) ([]Dependency, map[Dependency][]Dependency, error) {
	var filteredLines []string
	start := regexp.MustCompile("^\\[INFO\\] --- .*? ---$")
	started := false
	r := regexp.MustCompile("^\\[INFO\\] ([ `+\\\\|-]*)([^ `+\\\\|-].+)$")
	for _, line := range strings.Split(stdin, "\n") {
		if line == "[INFO] " || line == "[INFO] ------------------------------------------------------------------------" {
			started = false
		}
		if started {
			filteredLines = append(filteredLines, line)
		}
		if start.MatchString(line) {
			started = true
		}
	}
	// Remove first line, which is just the direct dependency.
	filteredLines = filteredLines[1:]

	depRegex := regexp.MustCompile("([^:]+):([^:]+):([^:]*):([^:]+)")
	return ReadDependencyTree(filteredLines, func(line string) (int, Dependency, error) {
		log.Logger.Debugf("line: %#v", line)
		matches := r.FindStringSubmatch(line)
		depth := len(matches[1])
		if depth%3 != 0 {
			// Sanity check
			log.Logger.Panicf("Bad depth: %#v %s %#v", depth, line)
		}
		level := depth / 3
		depMatches := depRegex.FindStringSubmatch(matches[2])
		name := depMatches[1] + ":" + depMatches[2]
		revision := depMatches[4]
		failed := false
		if strings.HasSuffix(revision, " FAILED") {
			revision = strings.TrimSuffix(revision, " FAILED")
			failed = true
		}

		return level, Dependency{Name: name, Version: revision, Failed: failed}, nil
	})
}
