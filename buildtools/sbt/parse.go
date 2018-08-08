package sbt

import (
	"regexp"
	"strings"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/pkg"
)

type GraphML struct {
	Graph Graph `xml:"graph"`
}

type Graph struct {
	Nodes []Node `xml:"node"`
	Edges []Edge `xml:"edge"`
}

type Node struct {
	ID string `xml:"id,attr"`
}

type Edge struct {
	Source string `xml:"source,attr"`
	Target string `xml:"target,attr"`
}

func ParseDependencyGraph(graph Graph, evicted string) (pkg.Imports, pkg.Deps, error) {
	log.Logger.Debugf("%#v %#v", graph, evicted)

	replacements := ParseEvicted(evicted)

	deps := make(map[pkg.ID]pkg.Imports)
	for _, edge := range graph.Edges {
		source := ParsePackageID(edge.Source)
		target := ParsePackageID(edge.Target)

		log.Logger.Debugf("source: %#v", source)

		_, ok := deps[source]
		if !ok {
			deps[source] = pkg.Imports{}
		}

		replacement, ok := replacements[target]
		if ok {
			target = replacement
		}

		deps[source] = append(deps[source], pkg.Import{
			Target:   target.String(),
			Resolved: target,
		})
	}

	pkgs := make(pkg.Deps)
	for id, imports := range deps {
		pkgs[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	root := ParsePackageID(graph.Nodes[0].ID)
	imports := deps[root]
	delete(pkgs, root)

	return imports, pkgs, nil
}

func ParseEvicted(evicted string) map[pkg.ID]pkg.ID {
	replacements := make(map[pkg.ID]pkg.ID)
	r := regexp.MustCompile("^\\[info\\] 	\\* (.*?) is selected over (.*?)$")
	for _, line := range strings.Split(evicted, "\n") {
		matches := r.FindStringSubmatch(line)
		if matches != nil {
			target := ParsePackageID(matches[1])
			if strings.HasPrefix(matches[2], "{") && strings.HasSuffix(matches[2], "}") {
				// Handle multiple versions.
				versions := strings.Split(strings.Trim(matches[2], "{}"), ", ")
				for _, version := range versions {
					source := target
					source.Revision = version
					replacements[source] = target
				}
			} else {
				// Handle single version.
				source := target
				source.Revision = matches[2]
				replacements[source] = target
			}
		}
	}
	return replacements
}

func ParsePackageID(packageID string) pkg.ID {
	r := regexp.MustCompile("([^:\\s]+):([^:\\s]+):([^:\\s]+).*")
	matches := r.FindStringSubmatch(packageID)
	return pkg.ID{
		Type:     pkg.Scala,
		Name:     matches[1] + ":" + matches[2],
		Revision: matches[3],
	}
}
