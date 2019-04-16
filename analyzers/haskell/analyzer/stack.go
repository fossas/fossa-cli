package analyzer

import (
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"strconv"
	"strings"
)

type StackDep struct {
	Name    string
	Version string
}

func StackDepToCanonical(dep StackDep) string {
	return dep.Name + "-" + dep.Version
}

func StackDepFromCanonical(canonical string) StackDep {
	ix := strings.LastIndex(canonical, "-")
	return StackDep{
		Name: canonical[:ix],
		Version: canonical[ix+1:],
	}
}

// TODO: trying to emulate Optional with *int.. maybe not a good idea?
func GetStackDependencies(depth *int) ([]StackDep, error) {
	args := []string{"ls", "dependencies"}

	if depth != nil {
		args = append(args, "--depth", strconv.Itoa(*depth))
	}

	stdout, _, err := exec.Run(exec.Cmd{
		Name: "stack",
		Argv: args,
	})

	if err != nil {
		return nil, err
	}

	return ParseStackDependencies(stdout), nil
}

func ParseStackDependencies(output string) []StackDep {
	// Stack ls dependencies outputs deps in the form:
	// packageone 0.0.1.0
	// packagetwo 0.0.1.0
	// ...

	var deps []StackDep

	for _, line := range strings.Split(output, "\n") {
		var dep = strings.Split(line, " ")

		if len(dep) < 2 {
			continue
		}

		var name    = dep[0]
		var version = dep[1]

		deps = append(deps, StackDep{
			Name:    name,
			Version: version,
		})
	}

	return deps
}

func StackDepToPkgId(dep StackDep) pkg.ID {
	return pkg.ID{
		Type: pkg.Haskell,
		Name: dep.Name,
		Revision: dep.Version,
	}
}

func AnalyzeStackPure(stackAllDeps []StackDep, stackImmediateDeps []StackDep, depMap GhcPkgDeps) graph.Deps {
	// Our direct dependencies
	var depGraph = make(map[pkg.ID]pkg.Package)

	// Build out the full graph
	for _, stackDep := range stackAllDeps {
		pkgID := StackDepToPkgId(stackDep)

		var imports []pkg.Import

		for _, ghcPkgDep := range depMap[StackDepToCanonical(stackDep)] {
			imports = append(imports, pkg.Import{
				Resolved: StackDepToPkgId(StackDepFromCanonical(ghcPkgDep)),
			})
		}

		depGraph[pkgID] = pkg.Package{
			ID: pkgID,
			Imports: imports,
		}
	}

	var directImports []pkg.Import

	// Build our direct dependencies
	for _, stackDep := range stackImmediateDeps {
		pkgID := StackDepToPkgId(stackDep)

		directImports = append(directImports, pkg.Import{
			Resolved: pkgID,
		})
	}

	return graph.Deps{
		Direct: directImports,
		Transitive: depGraph,
	}
}

func AnalyzeStack(_ module.Module) (graph.Deps, error) {

	// Get all deps from
	stackAllDeps, err := GetStackDependencies(nil)

	if err != nil {
		return graph.Deps{}, err
	}

	ghcPkgDeps, err := GetGhcPkgDepMap()

	if err != nil {
		return graph.Deps{}, err
	}

	depth := int(1)
	stackImmediateDeps, err := GetStackDependencies(&depth)

	if err != nil {
		return graph.Deps{}, err
	}

	return AnalyzeStackPure(stackAllDeps, stackImmediateDeps, ghcPkgDeps), nil
}

type GhcPkgDeps = map[string][]string

// TODO: move elsewhere
func GetGhcPkgDepMap() (GhcPkgDeps, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: "stack",
		Argv: []string{"exec", "--", "ghc-pkg", "dot"},
	})

	if err != nil {
		return nil, err
	}

	return ParseGhcPkgDepMap(stdout), nil
}

func ParseGhcPkgDepMap(output string) GhcPkgDeps {
	// ghc-pkg dot outputs deps in the form:
	// digraph {
	// "packageone-0.0.1.0" -> "packagetwo-0.0.1.0"
	// "packageone-0.0.1.0" -> "packagethree-0.0.1.0"
	// }
	deps := make(GhcPkgDeps)

	lines := strings.Split(output, "\n")

	for _, line := range lines {
		line = strings.ReplaceAll(line, "\"", "")

		split := strings.Split(line, " -> ")
		if len(split) < 2 {
			continue // The first and last lines are "digraph {" and "}", so they won't have a dep
		}

		cur := deps[split[0]]
		cur = append(cur, split[1])
		deps[split[0]] = cur
	}

	return deps
}

