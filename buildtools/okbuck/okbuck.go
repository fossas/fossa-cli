package okbuck

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// OkBuck defines an interface for all OkBuck tool implementations.
type OkBuck interface {
	Deps() (graph.Deps, error)
}

// Setup implements OkBuck and defines how to retrieve okbuck output.
type Setup struct {
	Target string
	Cmd    func(string, ...string) (string, error)
}

// Target represents an okbuck build target.
type Target struct {
	MavenCoordinates string `json:"mavenCoords"`
}

// Dependency holds revision information after parsing output.
type Dependency struct {
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

// Deps finds the dependencies of an OkBuck target using the supplied command and
// returns the dependency graph.
func (b Setup) Deps() (graph.Deps, error) {
	out, err := b.Cmd("targets", b.Target, "--json")
	if err != nil {
		return graph.Deps{}, err
	}

	var mavenCoordinates []Target
	err = json.Unmarshal([]byte(out), &mavenCoordinates)
	if err != nil {
		return graph.Deps{}, nil
	}

	imports := extractDependencies(mavenCoordinates)
	var direct []pkg.Import
	for _, dep := range imports {
		direct = append(direct, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Revision,
			},
		})
	}

	transitive := make(map[pkg.ID]pkg.Package)
	for _, dep := range imports {
		id := pkg.ID{
			Type:     pkg.Maven,
			Name:     dep.Name,
			Revision: dep.Revision,
		}
		transitive[id] = pkg.Package{
			ID:      id,
			Imports: []pkg.Import{},
		}
	}

	fullGraph := graph.Deps{
		Direct:     direct,
		Transitive: transitive,
	}

	return fullGraph, nil
}

func extractDependencies(output []Target) []Dependency {
	var deps []Dependency
	for _, out := range output {
		tmp := strings.Split(out.MavenCoordinates, ":")
		if len(tmp) == 4 {
			deps = append(deps, Dependency{
				Name:     tmp[0] + ":" + tmp[1],
				Revision: tmp[3],
			})
		}
	}
	return deps
}
