package buck

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type buckOutput struct {
	outputMapping map[string][]string
}

// Deps returns the dependencies of a Buck project using the buck audit command
func Deps(target string) (graph.Deps, error) {
	// Upload the dependencies.
	locatorMap, err := uploadDeps(target)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Get the transitive dependency graph.
	transDeps, err := depGraph(target, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Get the direct dependencies.
	imports, err := directDeps(target, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}, nil
}

func uploadDeps(target string) (map[string]fossa.Locator, error) {
	locatorMap := make(map[string]fossa.Locator)
	depList, err := buckAudit("input", target)
	if err != nil {
		return locatorMap, err
	}

	// Upload files and map the corresponding locator to the full dep path.
	for dep, files := range depList.outputMapping {
		// Modify the dependency name to appease core.
		// Changes "//src/fossa/buildtools:buck" into "buildtools-buck"
		depSplit := strings.Split(dep, "/")
		dependency := strings.Replace(depSplit[len(depSplit)-1], ":", "-", 1)
		locator, err := fossa.UploadTarballDependencyFiles(files, dependency)
		if err != nil {
			return locatorMap, err
		}
		locatorMap[dep] = locator
	}

	return locatorMap, nil
}

func depGraph(target string, locatorMap map[string]fossa.Locator) (map[pkg.ID]pkg.Package, error) {
	fullGraph := make(map[pkg.ID]pkg.Package)
	depList, err := buckAudit("dependencies", target, "--transitive")
	if err != nil {
		return fullGraph, err
	}

	for _, dep := range depList.outputMapping[target] {
		transDeps, err := buckAudit("dependencies", dep)
		if err != nil {
			return fullGraph, err
		}
		var imports []pkg.Import

		for _, transDep := range transDeps.outputMapping[dep] {
			// Create the secondary level deps ID
			transID := pkg.ID{
				Type:     pkg.Raw,
				Name:     locatorMap[transDep].Project,
				Revision: locatorMap[transDep].Revision,
			}

			// Create the package
			transImport := pkg.Import{
				Target:   transDep,
				Resolved: transID,
			}

			// Add it to the transitive list
			imports = append(imports, transImport)
		}

		// Create the head deps id and add its imports to its package and add to the map
		id := pkg.ID{
			Type:     pkg.Raw,
			Name:     locatorMap[dep].Project,
			Revision: locatorMap[dep].Revision,
		}
		fullGraph[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	return fullGraph, nil
}

func directDeps(target string, locatorMap map[string]fossa.Locator) ([]pkg.Import, error) {
	imports := []pkg.Import{}
	deps, err := buckAudit("dependencies", target)
	if err != nil {
		return imports, err
	}

	for _, dep := range deps.outputMapping[target] {
		imports = append(imports, pkg.Import{
			Target: dep,
			Resolved: pkg.ID{
				Type:     pkg.Raw,
				Name:     locatorMap[dep].Project,
				Revision: locatorMap[dep].Revision,
			},
		})
	}

	return imports, nil
}

func buckAudit(cmd, target string, args ...string) (buckOutput, error) {
	var output buckOutput
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{"audit", cmd, "--json", target}, args...),
	})
	if err != nil {
		return output, errors.Wrapf(err, "Could not run `buck audit %s --json %s` within the current directory", cmd, target)
	}

	err = json.Unmarshal([]byte(out), &output.outputMapping)
	if err != nil {
		return output, errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}

	return output, nil
}
