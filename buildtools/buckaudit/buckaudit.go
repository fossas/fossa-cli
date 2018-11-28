package buckaudit

import (
	"encoding/json"

	"github.com/apex/log"

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
func Deps(name string) (graph.Deps, error) {
	// Upload the dependencies.
	name = "//src/com/facebook/buck/jvm/java/lang/model:model"
	locatorMap, err := uploadDeps(name)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Get the transitive dependency graph.
	transDeps, err := depGraph(name, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Get the direct dependencies.
	imports, err := directDeps(name, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}, nil
}

func uploadDeps(pack string) (map[string]fossa.Locator, error) {
	locatorMap := make(map[string]fossa.Locator)
	depList, err := buckAudit("input", pack)
	if err != nil {
		return locatorMap, err
	}

	// Upload files and map the corresponding locator to the full dep path.
	for dep, files := range depList.outputMapping {
		locator, err := fossa.UploadTarballFiles(files, dep)
		if err != nil {
			log.Debugf("error uploading dependency: %s\n", dep)
			continue
		}
		locatorMap[dep] = locator
	}

	return locatorMap, nil
}

func depGraph(pack string, locatorMap map[string]fossa.Locator) (map[pkg.ID]pkg.Package, error) {
	fullGraph := make(map[pkg.ID]pkg.Package)
	depList, err := buckAudit("dependencies", pack, "--transitive")
	if err != nil {
		return fullGraph, err
	}

	for _, dep := range depList.outputMapping[pack] {
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
			pack := pkg.Import{
				Target:   transDep,
				Resolved: transID,
			}

			// Add it to the transitive list
			imports = append(imports, pack)
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

func directDeps(pack string, locatorMap map[string]fossa.Locator) ([]pkg.Import, error) {
	imports := []pkg.Import{}
	deps, err := buckAudit("dependencies", pack)
	if err != nil {
		return imports, err
	}

	for _, dep := range deps.outputMapping[pack] {
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

func buckAudit(cmd, pack string, args ...string) (buckOutput, error) {
	var output buckOutput
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{"audit", cmd, "--json", pack}, args...),
	})
	if err != nil {
		return output, errors.Wrapf(err, "Could not run `buck audit %s --json %s` within the current directory", cmd, pack)
	}

	err = json.Unmarshal([]byte(out), &output.outputMapping)
	if err != nil {
		return output, errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}

	return output, nil
}
