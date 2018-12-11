package graph

import (
	"github.com/cheekybits/genny/generic"
)

type Generic generic.Type

// level is 1-indexed
type LineParser func(line string) (level int, node Generic, err error)

func ReadGenericTree(lines []string, parser LineParser) ([]Generic, map[Generic][]Generic, error) {
	var imports []Generic
	edges := make(map[Generic]map[Generic]bool)
	parents := []Generic{}

	for _, line := range lines {
		level, node, err := parser(line)
		if err != nil {
			return nil, nil, err
		}

		// Add to graph.
		if len(parents) >= level {
			parents = parents[:level-1]
		}
		if level == 1 {
			imports = append(imports, node)
		} else {
			parent := parents[len(parents)-1]
			_, ok := edges[parent]
			if !ok {
				edges[parent] = make(map[Generic]bool)
			}
			edges[parent][node] = true
		}
		parents = append(parents, node)
	}

	graph := make(map[Generic][]Generic)
	for parent, children := range edges {
		for child := range children {
			graph[parent] = append(graph[parent], child)
		}
	}
	for _, i := range imports {
		_, ok := edges[i]
		if !ok {
			graph[i] = nil
		}
	}

	return imports, graph, nil
}
