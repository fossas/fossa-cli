package maven

import (
	"encoding/xml"
	"os"
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

// A MvnModule can identify a Maven project with the target path.
type MvnModule struct {
	// Name is taken from the module's POM file.
	Name string

	// Target is the relative path from the root of the FOSSA project to the module's manifest file or to
	// the directory containing the module's "pom.xml". The target may name a file other than "pom.xml" and
	// it may even be the groupId:artifactId string identifying the "Maven project".
	Target string

	// Dir is the relative path from the root of the FOSSA project to the module.
	Dir string
}

// Modules returns a list specifying the Maven module at path, which may name a file or directory, and all the
// Maven modules nested below it. The Target field of each MvnModule is set to the directory or manifest file
// describing the module. The visited manifest files are listed in the checked map.
func Modules(path string, checked map[string]bool) ([]MvnModule, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return nil, errors.Wrap(err, "could not get CWD")
	}

	dir := path
	pomFile := path
	if strings.HasSuffix(path, ".xml") {
		// We have the manifest file but still need its directory path.
		dir, err = filepath.Rel(cwd, filepath.Dir(path))
		if err != nil {
			return nil, errors.Wrapf(err, "could not get relative path to %q", path)
		}
	} else {
		// We have the directory and will assume it uses the standard name for the manifest file.
		pomFile = filepath.Join(path, "pom.xml")
	}

	if checked[pomFile] {
		return nil, nil
	}

	checked[pomFile] = true

	var pom Manifest
	if err := files.ReadXML(&pom, pomFile); err != nil {
		return nil, errors.Wrapf(err, "could not read POM file %q", pomFile)
	}

	modules := make([]MvnModule, 1, 1+len(pom.Modules))

	modules[0] = MvnModule{Name: pom.Name, Target: pomFile, Dir: dir}

	for _, module := range pom.Modules {
		childPath := filepath.Join(dir, module)
		children, err := Modules(childPath, checked)
		if err != nil {
			return nil, errors.Wrapf(err, "could not read child module at %q", childPath)
		}
		modules = append(modules, children...)
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
	splitReg := regexp.MustCompile("\r?\n")
	for _, line := range splitReg.Split(stdin, -1) {
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
	if len(filteredLines) == 0 {
		return nil, nil, errors.New("error parsing lines")
	}
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
