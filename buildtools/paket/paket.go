package paket

import (
	"fmt"

	"github.com/fossas/fossa-cli/graph"
)

func DependencyGraph(target string) (graph.Deps, error) {

	//ideally this is a mapping of map[Name]Dependency where Dependency is
	// Name, Revision, and Imports(string), and Type
	dependencyList := parseFile(target)
	for dep := range dependencyList {
		fmt.Println(dep)
	}

	return graph.Deps{}, nil
}

func parseFile(target string) []string {
	list := []string{}

	return list
}
