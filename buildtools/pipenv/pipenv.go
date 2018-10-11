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

// Exectable allows a custom dataAccess function to be used by Deps().
type Exectable interface {
	DataAccess(string) (string, error)
}

// SystemPipenv implements Pipenv.
type SystemPipenv struct {
	PipenvExec Exectable
}

// EnvironmentData implements pipenvExecutable and returns data output from pipenv.
type EnvironmentData struct{}

// dependency is used to unmarshall the output from pipenv graph and store
// an object representing an imported dependency.
type dependency struct {
	Package      string `json:"package_name"`
	Resolved     string `json:"installed_version"`
	Target       string `json:"required_version"`
	Dependencies []dependency
}

// New returns a new Pipenv object used for JSON data retrieval.
func New() Pipenv {
	return SystemPipenv{
		PipenvExec: EnvironmentData{},
	}
}

// Deps returns the list of imports and associated package graph
// using the data from p.PipenvExec.DataAccess.
func (p SystemPipenv) Deps() (graph.Deps, error) {
	depGraph := graph.Deps{}
	rawJSON, err := p.PipenvExec.DataAccess("")
	if err != nil {
		return depGraph, err
	}

	deps, err := getDependencies(rawJSON)
	if err != nil {
		return depGraph, err
	}

	depGraph.Direct = importsFromDependencies(deps)
	depGraph.Transitive = graphFromDependencies(deps)
	return depGraph, nil
}

func getDependencies(graphJSONFile string) ([]dependency, error) {
	var depList []dependency
	err := json.Unmarshal([]byte(graphJSONFile), &depList)
	if err != nil {
		return nil, errors.Wrap(err, "could not unmarshall JSON into dependency list")
	}
	return depList, nil
}

// DataAccess returns the output from `pipenv graph --json-tree`
func (data EnvironmentData) DataAccess(dirname string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "pipenv",
		Argv: []string{"graph", "--json-tree"},
		Dir:  dirname,
	})

	if err != nil {
		err = errors.Wrapf(err, "could not run `pipenv graph --json-tree` within the current directory: %+s", dirname)
	}
	return out, err
}

func importsFromDependencies(depList []dependency) []pkg.Import {
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

func graphFromDependencies(directDeps []dependency) map[pkg.ID]pkg.Package {
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
