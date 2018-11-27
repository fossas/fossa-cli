package buckaudit

import (
	"encoding/json"
	"fmt"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type dependencies struct {
	Deps map[string][]string
}

// Deps returns the dependencies of a Buck project using the buck audit command
// Define the package name when building the commands and make it part of a buck object for testing purposes
func Deps() (graph.Deps, error) {
	// Get the upload json
	//pkg := "//src/com/facebook/buck/cli/bootstrapper:bootstrapper"
	pkg := "//src/com/facebook/buck/jvm/java/lang/model:model"
	depPaths, err := deps(pkg)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Upload the deps
	fmt.Printf("\n------------------ Uploading Deps ---------------------------\n")
	revisionMap, err := uploadDeps(depPaths)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Make the graph
	fmt.Println("\n-------------- Making Transitive Graph ----------------------\n")
	transDeps, err := depGraph(pkg, revisionMap)
	if err != nil {
		return graph.Deps{}, nil
	}
	fmt.Println("\n------------- Making Direct Import List ----------------------\n")
	imports, err := directDeps(pkg, revisionMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}, nil
}

func deps(pkg string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: []string{"audit", "input", "--json", pkg},
	})
	if err != nil {
		err = errors.Wrapf(err, "Could not run `buck audit input --json %s` within the current directory", pkg)
	}
	return out, err
}

func uploadDeps(depJSON string) (map[string]fossa.Locator, error) {
	var depList dependencies
	revisionMap := make(map[string]fossa.Locator)
	err := json.Unmarshal([]byte(depJSON), &depList.Deps)
	if err != nil {
		return revisionMap, errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}

	for dep, files := range depList.Deps {
		locator, err := fossa.UploadTarballFiles(files, dep)
		fmt.Printf("\nfiles uploading %+v\n", files)
		fmt.Printf("locator: %s\nerror: %+v\n", locator, err)
		revisionMap[dep] = locator
	}

	// Tar all of the source files up and upload each individually
	return revisionMap, nil
}

func depGraph(pack string, revisionMap map[string]fossa.Locator) (map[pkg.ID]pkg.Package, error) {
	/* 	depList := buckAudit("buck", "--transitive")
	 */
	fullGraph := make(map[pkg.ID]pkg.Package)
	depGraph := buckAudit(pack, "--transitive")
	fmt.Println(depGraph)

	for _, dep := range depGraph.Deps[pack] {
		deepDeps := buckAudit(dep)
		fmt.Printf("deep\n")
		fmt.Println(dep, deepDeps)
		var imports []pkg.Import

		fmt.Printf("Deep Deps%+v\n", deepDeps)
		for _, deepdep := range deepDeps.Deps[dep] {
			fmt.Println("DEEEEEP DEPS")
			// Create the secondary level deps ID
			deepID := pkg.ID{
				Type:     pkg.Raw,
				Name:     revisionMap[deepdep].Project,
				Revision: revisionMap[deepdep].Revision,
			}

			// Create the package
			pack := pkg.Import{
				Target:   deepdep,
				Resolved: deepID,
			}

			fmt.Printf("package %+v\n", pack)
			// Add it to the transitive list
			imports = append(imports, pack)
		}

		// Create the head deps id and add its imports to its package and add to the map
		id := pkg.ID{
			Type:     pkg.Raw,
			Name:     revisionMap[dep].Project,
			Revision: revisionMap[dep].Revision,
		}
		fullGraph[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		fmt.Printf("Transitive ID: %+v\n", id)
	}

	return fullGraph, nil
}

func directDeps(pack string, revisionMap map[string]fossa.Locator) ([]pkg.Import, error) {
	imports := []pkg.Import{}
	deps := buckAudit(pack)
	for _, dep := range deps.Deps[pack] {
		imports = append(imports, pkg.Import{
			Target: dep,
			Resolved: pkg.ID{
				Type:     pkg.Raw,
				Name:     revisionMap[dep].Project,
				Revision: revisionMap[dep].Revision,
			},
		})
	}

	return imports, nil
}

func buckAudit(pack string, args ...string) dependencies {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{"audit", "dependencies", "--json", pack}, args...),
	})

	if err != nil {
		err = errors.Wrapf(err, "Could not run `buck audit input --json %s` within the current directory", pack)
	}

	var depList dependencies
	err = json.Unmarshal([]byte(out), &depList.Deps)
	if err != nil {
		errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}
	// Tar all of the source files up and upload each individually

	return depList
}

// Ideas
// Commands that I'm running are really just `buck audit dependencies --json` +
// `project` + Maybe `--transitive`. Break this out into its own function so that I don't have to call it all the time. Also this can be used for testing if broken out.
//This should also take care of the JSON parsing as well
