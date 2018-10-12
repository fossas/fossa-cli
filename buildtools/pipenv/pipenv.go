package pipenv

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Pipenv enables python.go to use a custom Deps() function for testing.
type Pipenv interface {
	Deps() (graph.Deps, error)
}

// Executable is a proxy to the pipenv build tools data access functionality.
type Executable interface {
	GraphJSON() (string, error)
}

// PipenvCmd implements Pipenv.
type PipenvCmd struct {
	PipenvExec Executable
}

// EnvironmentData implements Executable and returns data output from pipenv.
type EnvironmentData struct{}

// dependency is used to unmarshall the output from `pipenv graph --json-tree`
// and store an object representing an imported dependency.
type dependency struct {
	Package      string `json:"package_name"`
	Resolved     string `json:"installed_version"`
	Target       string `json:"required_version"`
	Dependencies []dependency
}

// New returns a new Pipenv instance that controls data access.
func New() Pipenv {
	return PipenvCmd{
		PipenvExec: EnvironmentData{},
	}
}

// Deps returns the list of imports and associated package graph
// using the data from p.PipenvExec.DataAccess.
func (p PipenvCmd) Deps() (graph.Deps, error) {
	depGraph := graph.Deps{}
	rawJSON, err := p.PipenvExec.GraphJSON()
	if err != nil {
		return depGraph, err
	}

	deps, err := getDependencies(rawJSON)
	if err != nil {
		return depGraph, err
	}

	depGraph.Direct = getDirectDeps(deps)
	depGraph.Transitive = getTransitiveDeps(deps)
	return depGraph, nil
}

func getDependencies(graphJSONFile string) ([]dependency, error) {
	var depList []dependency
	err := json.Unmarshal([]byte(graphJSONFile), &depList)
	if err != nil {
		return nil, errors.Wrap(err, "Could not unmarshall JSON into dependency list")
	}
	return depList, nil
}

// GraphJSON returns the output from `pipenv graph --json-tree`.
func (data EnvironmentData) GraphJSON() (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "pipenv",
		Argv: []string{"graph", "--json-tree"},
	})
	if err != nil {
		err = errors.Wrap(err, "Could not run `pipenv graph --json-tree` within the current directory")
	}
	return out, err
}

func getDirectDeps(depList []dependency) []pkg.Import {
	var imports []pkg.Import
	for _, dep := range depList {
		imports = append(imports, pkg.Import{
			Target: dep.Target,
			Resolved: pkg.ID{
				Type:     pkg.Python,
				Name:     dep.Package,
				Revision: dep.Resolved,
			},
		})
	}
	return imports
}

func getTransitiveDeps(directDeps []dependency) map[pkg.ID]pkg.Package {
	graph := make(map[pkg.ID]pkg.Package)
	for _, dep := range directDeps {
		id := pkg.ID{
			Type:     pkg.Python,
			Name:     dep.Package,
			Revision: dep.Resolved,
		}

		graph[id] = pkg.Package{
			ID:      id,
			Imports: packageImports(dep.Dependencies),
		}

		flattenDeepDependencies(graph, dep)
	}

	return graph
}

func flattenDeepDependencies(graph map[pkg.ID]pkg.Package, transDep dependency) {
	for _, dep := range transDep.Dependencies {
		id := pkg.ID{
			Type:     pkg.Python,
			Name:     dep.Package,
			Revision: dep.Resolved,
		}
		// Don't process duplicate transitive dependencies.
		_, ok := graph[id]
		if ok {
			continue
		}

		graph[id] = pkg.Package{
			ID:      id,
			Imports: packageImports(dep.Dependencies),
		}
		flattenDeepDependencies(graph, dep)
	}
}

func packageImports(packageDeps []dependency) []pkg.Import {
	var imports []pkg.Import
	for _, i := range packageDeps {
		imports = append(imports, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Python,
				Name:     i.Package,
				Revision: i.Resolved,
			},
		})
	}
	return imports
}
