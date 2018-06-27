package graph

import (
	"errors"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

var (
	ErrSkipLine = errors.New("ReadTree: skip line")
	Root        = struct{}{}
)

type LineParser func(line string) (level int, node interface{}, err error)

func ReadTree(lines string, parser LineParser) (Rooted, error) {
	edges := make(map[interface{}]map[interface{}]bool)
	parents := []interface{}{Root}

	for _, line := range strings.Split(string(lines), "\n") {
		level, node, err := parser(line)
		if err == ErrSkipLine {
			continue
		}
		if err != nil {
			return Rooted{}, err
		}

		// Add to graph.
		parents = parents[:level]
		parent := parents[len(parents)-1]
		_, ok := edges[parent]
		if !ok {
			edges[parent] = make(map[interface{}]bool)
		}
		edges[parent][node] = true
		parents = append(parents, node)
	}

	var imports []interface{}
	graph := make(map[interface{}][]interface{})
	for parent, children := range edges {
		for child := range children {
			if parent == Root {
				imports = append(imports, child)
			} else {
				graph[parent] = append(graph[parent], child)
			}
		}
	}

	log.Logger.Debugf("imports: %#v", imports)
	log.Logger.Debugf("graph: %#v", graph)
	return Rooted{
		Imports: imports,
		Graph:   graph,
	}, nil
}
