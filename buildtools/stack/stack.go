package stack

import (
	"strconv"
	"strings"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// ----- Dep graph retrieval

func GetDeps(m module.Module) (graph.Deps, error) {
	stackAllDeps, err := GetStackDependencies(m.Dir, 0)
	if err != nil {
		return graph.Deps{}, err
	}

	ghcPkgDeps, err := GetGhcPkgDepMap()
	if err != nil {
		return graph.Deps{}, err
	}

	stackImmediateDeps, err := GetStackDependencies(m.Dir, 1)
	if err != nil {
		return graph.Deps{}, err
	}

	return GetDepsPure(stackAllDeps, stackImmediateDeps, ghcPkgDeps), nil
}

func GetDepsPure(stackAllDeps []Dep, stackImmediateDeps []Dep, depMap GhcPkgDeps) graph.Deps {
	// Our direct dependencies
	var depGraph = make(map[pkg.ID]pkg.Package)

	// Build out the full graph
	for _, stackDep := range stackAllDeps {
		pkgID := DepToPkgId(stackDep)

		var imports []pkg.Import

		for _, ghcPkgDep := range depMap[DepToCanonical(stackDep)] {
			imports = append(imports, pkg.Import{
				Resolved: DepToPkgId(DepFromCanonical(ghcPkgDep)),
			})
		}

		depGraph[pkgID] = pkg.Package{
			ID:      pkgID,
			Imports: imports,
		}
	}

	var directImports []pkg.Import

	// Build our direct dependencies
	for _, stackDep := range stackImmediateDeps {
		pkgID := DepToPkgId(stackDep)

		directImports = append(directImports, pkg.Import{
			Resolved: pkgID,
		})
	}

	return graph.Deps{
		Direct:     directImports,
		Transitive: depGraph,
	}
}

// ----- Types

// A stack dependency
type Dep struct {
	Name    string
	Version string
}

func DepToPkgId(dep Dep) pkg.ID {
	return pkg.ID{
		Type:     pkg.Haskell,
		Name:     dep.Name,
		Revision: dep.Version,
	}
}

// Canonical string representation of a ghc-pkg package of the form:
// package-name-0.0.1.0
type Canonical struct {
	Identifier string
}

func DepToCanonical(dep Dep) Canonical {
	return Canonical{Identifier: dep.Name + "-" + dep.Version}
}

func DepFromCanonical(canonical Canonical) Dep {
	ix := strings.LastIndex(canonical.Identifier, "-")
	return Dep{
		Name:    canonical.Identifier[:ix],
		Version: canonical.Identifier[ix+1:],
	}
}

// A mapping of ghc-pkg packages to their dependencies
type GhcPkgDeps = map[Canonical][]Canonical

// ----- Command output parsing

func ParseStackDependencies(output string) []Dep {
	// Stack ls dependencies outputs deps in the form:
	// packageone 0.0.1.0
	// packagetwo 0.0.1.0
	// ...

	var deps []Dep

	for _, line := range strings.Split(output, "\n") {
		var dep = strings.Split(line, " ")

		if len(dep) < 2 {
			continue
		}

		var name = dep[0]
		var version = dep[1]

		deps = append(deps, Dep{
			Name:    name,
			Version: version,
		})
	}

	return deps
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

		from := Canonical{Identifier: split[0]}
		to := Canonical{Identifier: split[1]}

		cur := deps[from]
		cur = append(cur, to)
		deps[from] = cur
	}

	return deps
}

// ----- Command invocation

func GetStackDependencies(dir string, depth int) ([]Dep, error) {
	args := []string{"ls", "dependencies"}

	if depth != 0 {
		args = append(args, "--depth", strconv.Itoa(depth))
	}

	stdout, _, err := exec.Run(exec.Cmd{
		Name: "stack",
		Argv: args,
		Dir:  dir,
	})
	if err != nil {
		return nil, err
	}

	return ParseStackDependencies(stdout), nil
}

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
