package gradle

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
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

func (g *Gradle) ProjectListDependencies(projects []string) (map[string]graph.Deps, error) {
	configurationMap := make(map[string]graph.Deps)
	for _, project := range projects {
		depGraph, err := g.Dependencies(project)
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

func (g *Gradle) Dependencies(project string) (map[string]graph.Deps, error) {
	args := []string{
		project + ":dependencies",
		"--quiet",
	}
	if !g.Online {
		args = append(args, "--offline")
	}

	return g.DependenciesTask(args...)
}

func (g *Gradle) DependenciesTask(taskArgs ...string) (map[string]graph.Deps, error) {
	stdout, err := g.Run(taskArgs...)
	if err != nil {
		return nil, err
	}

	// Parse individual configurations.
	configurations := make(map[string]graph.Deps)
	// Divide output into configurations. Each configuration is separated by an
	// empty line, started with a configuration name (and optional description),
	// and has a dependency as its second line.
	sections := strings.Split(stdout, "\n\n")
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
			// TODO: the desired method for handling this might be to recurse into the subproject.
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

func NormalizeDependencies(imports []Dependency, deps map[Dependency][]Dependency) graph.Deps {
	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Target: dep.Target,
			Resolved: pkg.ID{
				Type:     pkg.Gradle,
				Name:     dep.Name,
				Revision: dep.Resolved,
			},
		})
	}

	// Set transitive dependencies.
	d := make(map[pkg.ID]pkg.Package)
	for parent, children := range deps {
		id := pkg.ID{
			Type:     pkg.Gradle,
			Name:     parent.Name,
			Revision: parent.Resolved,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.Resolved,
				Resolved: pkg.ID{
					Type:     pkg.Gradle,
					Name:     child.Name,
					Revision: child.Resolved,
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
