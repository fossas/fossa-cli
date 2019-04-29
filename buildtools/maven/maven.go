package maven

import (
	"encoding/xml"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/pkg"
)

type Manifest struct {
	Project      xml.Name     `xml:"project"`
	Parent       Parent       `xml:"parent"`
	Modules      []string     `xml:"modules>module"`
	ArtifactID   string       `xml:"artifactId"`
	GroupID      string       `xml:"groupId"`
	Version      string       `xml:"version"`
	Description  string       `xml:"description"`
	Name         string       `xml:"name"`
	URL          string       `xml:"url"`
	Dependencies []Dependency `xml:"dependencies>dependency"`
}

type Parent struct {
	ArtifactID string `xml:"artifactId"`
	GroupID    string `xml:"groupId"`
	Version    string `xml:"version"`
}

type Dependency struct {
	Name string

	GroupId    string `xml:"groupId"`
	ArtifactId string `xml:"artifactId"`
	Version    string `xml:"version"`

	// Scope is where the dependency is used, such as "test" or "runtime".
	Scope string `xml:"scope"`

	Failed bool
}

type depsList []Dependency

func (d depsList) toImports() []pkg.Import {
	i := make([]pkg.Import, 0, len(d))
	for _, dep := range d {
		i = append(i, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Version,
			},
		})
	}
	return i
}

// ToGraphDeps returns simply the list of dependencies listed within the manifest file.
func (m *Manifest) ToGraphDeps() ([]pkg.Import, map[pkg.ID]pkg.Package) {
	imports := depsList(m.Dependencies).toImports()

	// From just a POM file we don't know what really depends on what, so list all imports in the graph.
	g := make(map[pkg.ID]pkg.Package)
	for _, dep := range m.Dependencies {
		pack := pkg.Package{
			ID: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Version,
			},
		}
		g[pack.ID] = pack
	}

	return imports, g
}

type depsMap map[Dependency][]Dependency

func (d depsMap) toPkgGraph() map[pkg.ID]pkg.Package {
	g := make(map[pkg.ID]pkg.Package)
	for parent, children := range d {
		pack := pkg.Package{
			ID: pkg.ID{
				Type:     pkg.Maven,
				Name:     parent.Name,
				Revision: parent.Version,
			},
			Imports: make([]pkg.Import, 0, len(children)),
		}
		for _, child := range children {
			pack.Imports = append(pack.Imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Maven,
					Name:     child.Name,
					Revision: child.Version,
				},
			})
		}
		g[pack.ID] = pack
	}
	return g
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

// ResolveManifestFromBuildTarget tries to determine what buildTarget is supposed to be and then reads the POM
// manifest file pointed to by buildTarget if it is a path to such a file or module.
func (m *Maven) ResolveManifestFromBuildTarget(buildTarget string) (*Manifest, error) {
	var pomFile string
	stat, err := os.Stat(buildTarget)
	if err != nil {
		// buildTarget is not a path.
		if strings.Count(buildTarget, ":") == 1 {
			// This is likely a module ID.
			return nil, fmt.Errorf("cannot identify POM file for module %q", buildTarget)
		}
		return nil, fmt.Errorf("manfist file for %q cannot be read", buildTarget)
	}
	if stat.IsDir() {
		// We have the directory and will assume it uses the standard name for the manifest file.
		pomFile = filepath.Join(buildTarget, "pom.xml")
	} else {
		// We have the manifest file but still need its directory path.
		pomFile = buildTarget
	}

	var pom Manifest
	if err := files.ReadXML(&pom, pomFile); err != nil {
		return nil, err
	}
	return &pom, nil
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
func Modules(path string, baseDir string, checked map[string]bool) ([]MvnModule, error) {
	dir := path
	pomFile := path
	stat, err := os.Stat(path)
	if err != nil {
		return nil, errors.Wrapf(err, "could not check type of %q", path)
	}
	if stat.IsDir() {
		// We have the directory and will assume it uses the standard name for the manifest file.
		pomFile = filepath.Join(path, "pom.xml")
	} else {
		// We have the manifest file but still need its directory path.
		dir = filepath.Dir(path)
	}

	absPath, err := filepath.Abs(path)
	if err != nil {
		return nil, errors.Wrapf(err, "could not get absolute path of %q", absPath)
	}

	if checked[absPath] {
		return nil, nil
	}

	checked[pomFile] = true

	var pom Manifest
	if err := files.ReadXML(&pom, pomFile); err != nil {
		return nil, errors.Wrapf(err, "could not read POM file %q", pomFile)
	}

	modules := make([]MvnModule, 1, 1+len(pom.Modules))

	name := pom.Name
	if name == "" {
		name = pom.ArtifactID
	}

	modules[0] = MvnModule{Name: name, Target: pomFile, Dir: dir}

	for _, module := range pom.Modules {
		childPath := filepath.Join(dir, module)
		children, err := Modules(childPath, baseDir, checked)
		if err != nil {
			return nil, errors.Wrapf(err, "could not read child module at %q", childPath)
		}
		modules = append(modules, children...)
	}

	return modules, nil
}

// DependencyList runs Maven's dependency:list goal for the specified project.
func (m *Maven) DependencyList(dir, buildTarget string) (string, error) {
	return m.tryDependencyCommands("list", dir, buildTarget)
}

// DependencyTree runs Maven's dependency:tree goal for the specified project.
func (m *Maven) DependencyTree(dir, buildTarget string) ([]pkg.Import, map[pkg.ID]pkg.Package, error) {
	output, err := m.tryDependencyCommands("tree", dir, buildTarget)
	if err != nil {
		return nil, nil, err
	}
	return ParseDependencyTree(output)
}

func (m *Maven) tryDependencyCommands(subGoal, dir, buildTarget string) (stdout string, err error) {
	goalArgs := []string{"dependency:" + subGoal, "--batch-mode"}
	cmd := exec.Cmd{Name: m.Cmd, Dir: dir}

	// First, we try with the buildTarget because we have no idea what it is or whether the reactor already
	// knows about the module.
	cmd.Argv = append(goalArgs, "--projects", buildTarget)
	output, _, err := exec.Run(cmd)
	if err != nil {
		// Now we try to identify the groupId:artifactId identifier for the module and specify the path to
		// the manifest file directly.
		pom, err2 := m.ResolveManifestFromBuildTarget(buildTarget)
		if err2 != nil {
			// Using buildTarget as a module ID or as a path to a manifest did not work.
			// Return just the error from running the mvn goal the first time.
			return "", errors.Wrap(err, "could not use Maven to list dependencies")
		}

		// At this point we still don't know if buildTarget is the path to a directory or to a manifest file,
		// so create pomFilePath based on the file extension.
		pomFilePath := buildTarget
		if !strings.HasSuffix(pomFilePath, ".xml") {
			pomFilePath = filepath.Join(pomFilePath, "pom.xml")
		}

		cmd.Argv = append(goalArgs, "--projects", pom.GroupID+":"+pom.ArtifactID, "--file", pomFilePath)

		output, _, err2 = exec.Run(cmd)
		err = errors.Wrapf(err2, "could not run %s (original error: %v)", goalArgs[0], err)
	}
	return output, err
}

//go:generate bash -c "genny -in=$GOPATH/src/github.com/fossas/fossa-cli/graph/readtree.go gen 'Generic=Dependency' | sed -e 's/package graph/package maven/' > readtree_generated.go"

func ParseDependencyTree(stdin string) ([]pkg.Import, map[pkg.ID]pkg.Package, error) {
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
	imports, deps, err := ReadDependencyTree(filteredLines, func(line string) (int, Dependency, error) {
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
	if err != nil {
		return nil, nil, err
	}
	return depsList(imports).toImports(), depsMap(deps).toPkgGraph(), nil
}
