package okbuck

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// OkBuck defines an interface for all OkBuck tool implementations.
type OkBuck interface {
	Deps(string) (graph.Deps, error)
}

// Setup implements OkBuck and defines how to retrieve OkBuck output.
type Setup struct {
	Target string
	Cmd    func(...string) (string, error)
}

// Target represents an OkBuck build target.
type Target struct {
	BinaryJar        string `json:"binaryJar"`
	Aar              string `json:"aar"`
	MavenCoordinates string `json:"mavenCoords"`
}

// Dependency holds revision information after parsing output.
type Dependency struct {
	Jar      string
	Name     string
	Revision string
}

// New creates a new OkBuck instance that calls the okbuck build tool directly.
func New(target string) OkBuck {
	return Setup{
		Target: target,
		Cmd:    Cmd,
	}
}

// Deps finds the dependencies of an OkBuck target or classpath using
// the supplied command and returns the dependency graph.
func (b Setup) Deps(classpath string) (graph.Deps, error) {
	out, err := b.Cmd("targets", b.Target, "--json")
	if err != nil {
		return graph.Deps{}, err
	}

	var targets []Target
	err = json.Unmarshal([]byte(out), &targets)
	if err != nil {
		return graph.Deps{}, err
	}

	dependencies := depsFromTargets(targets)

	if classpath != "" {
		out, err := b.Cmd("audit", "classpath", classpath)
		if err != nil {
			return graph.Deps{}, err
		}
		jars := classpathJars(out)
		return graphFromJars(jars, dependencies), nil
	}

	depGraph := graph.Deps{
		Direct:     []pkg.Import{},
		Transitive: make(map[pkg.ID]pkg.Package),
	}
	for _, dep := range dependencies {
		id := pkg.ID{
			Type:     pkg.Maven,
			Name:     dep.Name,
			Revision: dep.Revision,
		}
		depGraph.Direct = append(depGraph.Direct, pkg.Import{Resolved: id})
		depGraph.Transitive[id] = pkg.Package{
			ID:      id,
			Imports: []pkg.Import{},
		}
	}

	return depGraph, nil
}

// depsFromTargets takes an OkBuck target list and creates a list of
// Dependencies that can be used to build a graph.Deps object.
func depsFromTargets(targets []Target) []Dependency {
	var deps []Dependency
	for _, target := range targets {
		splitDep := strings.Split(target.MavenCoordinates, ":")
		if len(splitDep) == 4 {
			deps = append(deps, Dependency{
				Jar:      conditionJar(target),
				Name:     splitDep[0] + ":" + splitDep[1],
				Revision: splitDep[3],
			})
		}
	}
	return deps
}

// classpathJars reads output from `buck audit classpath` and
// finds the unique jar identifies. Examples:
// .okbuck/android/constraint/__package.aar#aar_prebuilt_jar__/classes.jar --> package.aar
// .okbuck/android/constraint/__package.jar__/package.jar --> package.jar
func classpathJars(cmdOut string) []string {
	jarList := strings.Split(cmdOut, "\n")
	deps := []string{}
	for _, jar := range jarList {
		jarFromLine := strings.Split(jar, "_")
		if len(jarFromLine) >= 3 {
			jarFromLine = strings.Split(jarFromLine[2], "#")
		}
		deps = append(deps, jarFromLine[0])
	}
	return deps
}

func graphFromJars(jarList []string, dependencies []Dependency) graph.Deps {
	depGraph := graph.Deps{
		Direct:     []pkg.Import{},
		Transitive: make(map[pkg.ID]pkg.Package),
	}

	jarMap := mapJars(dependencies)
	for _, jar := range jarList {
		dep := jarMap[jar]
		if dep != (Dependency{}) {
			id := pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Revision,
			}
			depGraph.Direct = append(depGraph.Direct, pkg.Import{Resolved: id})
			depGraph.Transitive[id] = pkg.Package{
				ID:      id,
				Imports: []pkg.Import{},
			}
		}
	}
	return depGraph
}

// mapJars creates a mapping of a jar to the full Dependency.
func mapJars(dependencies []Dependency) map[string]Dependency {
	jarMap := make(map[string]Dependency)
	for _, dependency := range dependencies {
		jarMap[dependency.Jar] = dependency
	}
	return jarMap
}

// conditionJar determines what an OkBuck targets jar is.
// "aar" : ":package.aar__downloaded" -> package.aar
// "binaryJar" : ":package.jar__downloaded" -> package.jar
func conditionJar(target Target) string {
	jar := target.BinaryJar
	if target.Aar != "" {
		jar = target.Aar
	}
	jar = strings.TrimPrefix(jar, ":")
	jar = strings.Split(jar, "_")[0]
	return jar
}
