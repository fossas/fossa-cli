package maven

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

	// Dir is the relative path from the root of the FOSSA project to either the module or to a parent module
	// under which this one is listed. This is where `mvn` should be run to get the dependency tree.
	Dir string
}

// Modules returns a list of all Maven modules at the directory of pomFilePath and below.
func Modules(pomFilePath string, reactorDir string, checked map[string]bool) ([]MvnModule, error) {
	absPath, err := filepath.Abs(pomFilePath)
	if err != nil {
		return nil, errors.Wrapf(err, "could not get absolute path of %q", pomFilePath)
	}

	if checked[absPath] {
		return nil, nil
	}

	checked[absPath] = true

	var pom Manifest
	if err := files.ReadXML(&pom, absPath); err != nil {
		return nil, errors.Wrapf(err, "could not read POM file %q", absPath)
	}

	modules := make([]MvnModule, 1, 1+len(pom.Modules))

	name := pom.Name
	if name == "" {
		name = pom.ArtifactID
	}

	pomRelative, err := filepath.Rel(reactorDir, pomFilePath)
	if err != nil {
		return nil, errors.Wrapf(err, "could not determine relative path of %q from %q", pomFilePath, reactorDir)
	}

	modules[0] = MvnModule{Name: name, Target: pomRelative, Dir: reactorDir}

	pomDir := filepath.Dir(pomFilePath)

	for _, module := range pom.Modules {
		childPath := filepath.Join(pomDir, module)
		childStat, err := os.Stat(childPath)
		if err != nil {
			return nil, errors.Wrapf(err, "could not check type of %q", childPath)
		}
		if childStat.IsDir() {
			// Assume the listed module uses the standard name for the manifest file.
			childPath = filepath.Join(childPath, "pom.xml")
		}
		children, err := Modules(childPath, pomDir, checked)
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
func (m *Maven) DependencyTree(dir, buildTarget string) (graph.Deps, error) {
	output, err := m.tryDependencyCommands("tree", dir, buildTarget)
	if err != nil {
		return graph.Deps{}, err
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
		pom, err2 := ResolveManifestFromTarget(buildTarget, dir)
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

func ParseDependencyTree(stdin string) (graph.Deps, error) {
	var modules [][]string
	var filteredLines []string
	start := regexp.MustCompile(`^\[INFO\] --- .*? ---$`)
	started := false
	r := regexp.MustCompile("^\\[INFO\\] ([ `+\\\\|-]*)([^ `+\\\\|-].+)$")
	moduleBreak := regexp.MustCompile("^\\[INFO\\] -+<.+>-+")
	splitReg := regexp.MustCompile("\r?\n")
	for _, line := range splitReg.Split(stdin, -1) {
		if line == "[INFO]" || line == "[INFO] " || line == "[INFO] ------------------------------------------------------------------------" {
			started = false
		}
		if strings.HasPrefix(line, "[INFO] Downloading ") || strings.HasPrefix(line, "[WARNING]") {
			continue
		}
		if strings.HasPrefix(line, "Download") {
			continue
		}

		if moduleBreak.MatchString(line) && len(filteredLines) > 0 {
			filteredLines = filteredLines[1:]
			modules = append(modules, filteredLines)
			filteredLines = []string{}
			started = false
		}

		if started {
			filteredLines = append(filteredLines, line)
		}
		if start.MatchString(line) {
			started = true
		}
	}

	filteredLines = filteredLines[1:]
	modules = append(modules, filteredLines)

	depRegex := regexp.MustCompile("([^:]+):([^:]+):([^:]*):([^:]+)")

	totalImports := make(map[Dependency]bool)
	depGraphMap := make(map[Dependency]map[Dependency]bool)

	for _, moduleLines := range modules {
		imports, deps, err := ReadDependencyTree(moduleLines, func(line string) (int, Dependency, error) {
			log.WithField("line", line).Debug("parsing output line")
			matches := r.FindStringSubmatch(line)
			depth := len(matches[1])
			if depth%3 != 0 {
				// Sanity check
				log.WithField("depth", depth).Fatal("bad depth")
			}
			level := depth / 3
			depMatches := depRegex.FindStringSubmatch(matches[2])
			revision := depMatches[4]
			failed := false
			if strings.HasSuffix(revision, " FAILED") {
				revision = strings.TrimSuffix(revision, " FAILED")
				failed = true
			}

			dep := Dependency{GroupId: depMatches[1], ArtifactId: depMatches[2], Version: revision, Failed: failed}
			return level, dep, nil
		})
		if err != nil {
			return graph.Deps{}, err
		}

		for _, imp := range imports {
			totalImports[imp] = true
		}

		for dep, tree := range deps {
			if depGraphMap[dep] == nil {
				depGraphMap[dep] = make(map[Dependency]bool)
			}
			for _, d := range tree {
				depGraphMap[dep][d] = true
			}
		}
	}

	importList := []Dependency{}
	for i := range totalImports {
		importList = append(importList, i)
	}

	depGraphList := make(map[Dependency][]Dependency)
	for root, depMap := range depGraphMap {
		// depList ensures all dependencies are included in the resulting
		// graph even if they have 0 length depMaps.
		depList := []Dependency{}
		for dep := range depMap {
			depList = append(depList, dep)
		}
		depGraphList[root] = depList
	}

	return graph.Deps{
		Direct:     depsListToImports(importList),
		Transitive: depsMapToPkgGraph(depGraphList),
	}, nil
}

func depsListToImports(d []Dependency) []pkg.Import {
	i := make([]pkg.Import, 0, len(d))
	for _, dep := range d {
		i = append(i, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.ID(),
				Revision: dep.Version,
			},
		})
	}
	return i
}

func depsMapToPkgGraph(deps map[Dependency][]Dependency) map[pkg.ID]pkg.Package {
	pkgGraph := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		pack := pkg.Package{
			ID: pkg.ID{
				Type:     pkg.Maven,
				Name:     parent.ID(),
				Revision: parent.Version,
			},
			Imports: make([]pkg.Import, 0, len(children)),
		}
		for _, child := range children {
			pack.Imports = append(pack.Imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Maven,
					Name:     child.ID(),
					Revision: child.Version,
				},
			})
		}
		pkgGraph[pack.ID] = pack
	}
	return pkgGraph
}
