package bazel

import (
	"strings"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// GoBazelDependencies parses a Bazel file set up to exclusively handle
// go_repository blocks.
func GoBazelDependencies(target string) (graph.Deps, error) {
	buildFile, err := files.Read(target)
	if err != nil {
		return graph.Deps{}, err
	}

	var version, name string
	repoBlock := false
	depGraph := graph.Deps{Transitive: make(map[pkg.ID]pkg.Package)}
	for _, line := range strings.Split(string(buildFile), "\n") {
		trimLine := strings.TrimSpace(line)
		if strings.HasPrefix(trimLine, "go_repository") {
			repoBlock = true
		}

		if repoBlock {
			if strings.HasPrefix(trimLine, "importpath") {
				name = extractFieldValue(trimLine)
			}

			if strings.HasPrefix(trimLine, "version") {
				version = extractFieldValue(trimLine)
			}

			if strings.Contains(trimLine, ")") {
				if version != "" && name != "" {
					ID := pkg.ID{
						Type:     pkg.Go,
						Name:     name,
						Revision: version,
					}
					depGraph.Direct = append(depGraph.Direct, pkg.Import{Resolved: ID})
					depGraph.Transitive[ID] = pkg.Package{ID: ID}
				}

				repoBlock = false
				version = ""
				name = ""
			}
		}
	}

	return depGraph, nil
}

// extract `value` from `importpath = "value",`.
func extractFieldValue(line string) string {
	splitLine := strings.Split(line, " ")
	if len(splitLine) < 3 {
		return ""
	}

	return strings.Trim(strings.Trim(splitLine[2], ","), "\"")
}
