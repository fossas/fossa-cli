package maven

import (
	"encoding/xml"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
)

type Manifest struct {
	Project     xml.Name `xml:"project"`
	Parent      Parent   `xml:"parent"`
	Modules     []string `xml:"modules>module"`
	ArtifactID  string   `xml:"artifactId"`
	GroupID     string   `xml:"groupId"`
	Version     string   `xml:"version"`
	Description string   `xml:"description"`
	Name        string   `xml:"name"`
	URL         string   `xml:"url"`
}

type Parent struct {
	ArtifactID string `xml:"artifactId"`
	GroupID    string `xml:"groupId"`
	Version    string `xml:"version"`
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

func Modules(dir string) ([]string, error) {
	var pom Manifest
	err := files.ReadXML(&pom, filepath.Join(dir, "pom.xml"))
	if err != nil {
		return nil, errors.Wrap(err, "could not read POM file")
	}

	var modules []string
	for _, module := range pom.Modules {
		children, err := Modules(filepath.Join(dir, module))
		if err != nil {
			return nil, errors.Wrap(err, "could not read child modules")
		}
		modules = append(modules, children...)
	}

	groupID := pom.GroupID
	if groupID == "" {
		groupID = pom.Parent.GroupID
	}
	modules = append(modules, groupID+":"+pom.ArtifactID)

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
		if strings.HasPrefix(line, "[INFO] Downloading ") {
			continue
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
		log.WithField("line", line).Debug("parsing output line")
		matches := r.FindStringSubmatch(line)
		depth := len(matches[1])
		if depth%3 != 0 {
			// Sanity check
			log.WithField("depth", depth).Fatal("bad depth")
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
