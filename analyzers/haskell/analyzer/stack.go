package analyzer

import (
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"strings"
)

func AnalyzeStack(_ module.Module) (graph.Deps, error) {
	// Stack ls dependencies outputs deps in the form:
	// packageone 0.0.1.0
	// packagetwo 0.0.1.0
	// ...

	localDepsStdout, _, err := exec.Run(exec.Cmd{
		Name: "stack",
		Argv: []string{"ls", "dependencies", "--depth", "1"},
	})

	if err != nil {
		return graph.Deps{}, err
	}

	allDepsStdout, _, err := exec.Run(exec.Cmd{
		Name: "stack",
		Argv: []string{"ls", "dependencies"},
	})

	// Keep track of recorded packages so we don't include them twice
	var seen = make(map[string]bool)

	// Our direct dependencies
	var imports []pkg.Import

	FoldStackDeps(seen, localDepsStdout, func(name string, version string) {
		imports = append(imports, pkg.Import{
			// TODO: do we need to include Target?
			Resolved: pkg.ID{
				Type:     pkg.Haskell,
				Name:     name,
				Revision: version,
			},
		})
	})

	// Our transitive dependencies
	var transitive = make(map[pkg.ID]pkg.Package)

	FoldStackDeps(seen, allDepsStdout, func(name string, version string){
		pkgId := pkg.ID{
			Type: pkg.Haskell,
			Name: name,
			Revision: version,
		}

		transitive[pkgId] = pkg.Package{
			ID: pkgId,
		}
	})

	deps := graph.Deps{
		Direct: imports,
		Transitive: transitive,
	}

	return deps, nil
}

func FoldStackDeps(seen map[string]bool, depsOutput string, consume func(string, string)) {
	for _, line := range strings.Split(depsOutput, "\n") {
		var dep = strings.Split(line, " ")

		if len(dep) < 2 {
			continue
		}

		var name    = dep[0]
		var version = dep[1]

		// Add to imports if we haven't seen this dep already
		if _, ok := seen[line]; !ok {
			seen[line] = true

			consume(name,version)
		}
	}
}

