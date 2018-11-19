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

var TestDep = "//third-party/java/maven:maven-model"

// Deps returns the dependencies of a Buck project using the buck audit command
// Define the package name when building the commands and make it part of a buck object for testing purposes
func Deps() (graph.Deps, error) {
	// Get the upload json
	pkg := "//src/com/facebook/buck/core/module:module"
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

	// Get the graph json
	fmt.Println("\n------------- Getting Transitive Graph ----------------------\n")
	depGraphJSONOut, err := transDepsJSON(pkg)
	if err != nil {
		return graph.Deps{}, nil
	}

	// Make the graph
	fmt.Println("\n-------------- Making Transitive Graph ----------------------\n")
	transDeps, err := depGraph(depGraphJSONOut, revisionMap)
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

func uploadDeps(depJSON string) (map[string]string, error) {
	var depList dependencies
	revisionMap := make(map[string]string)
	err := json.Unmarshal([]byte(depJSON), &depList.Deps)
	if err != nil {
		return revisionMap, errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}

	for dep, files := range depList.Deps {
		locator, err := fossa.UploadTarballFiles(files, dep)
		fmt.Printf("\nfiles uploading %+v\n", files)
		fmt.Printf("locator: %s\nerror: %+v\n", locator, err)
		revisionMap[dep] = locator.Revision
	}

	// Tar all of the source files up and upload each individually
	return revisionMap, nil
}

func transDepsJSON(pkg string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: []string{"audit", "dependencies", "--json", "--transitive", pkg},
	})

	if err != nil {
		err = errors.Wrapf(err, "Could not run `buck audit input --json %s` within the current directory", pkg)
	}
	return out, err
}

func depGraph(graphJSON string, revisionMap map[string]string) (map[pkg.ID]pkg.Package, error) {
	/* 	depList := buckAudit("buck", "--transitive")
	 */fullGraph := make(map[pkg.ID]pkg.Package)
	deps := []string{TestDep}

	for _, dep := range deps {
		deepDeps := buckAudit(dep)
		fmt.Printf("deep\n")
		fmt.Println(dep, deepDeps)
		var imports []pkg.Import

		fmt.Printf("Deep Deps%+v\n", deepDeps)
		for _, deepdep := range deepDeps.Deps[TestDep] {
			fmt.Println("DEEEEEP DEPS")
			// Create the secondary level deps ID
			deepID := pkg.ID{
				Type:     pkg.Raw,
				Name:     deepdep,
				Revision: revisionMap[deepdep],
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
			Name:     dep,
			Revision: revisionMap[dep],
		}
		fullGraph[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		fmt.Printf("Transitive ID: %+v\n", id)
	}

	return fullGraph, nil
}

func directDeps(pack string, revisionMap map[string]string) ([]pkg.Import, error) {
	imports := []pkg.Import{
		pkg.Import{
			Target: TestDep,
			Resolved: pkg.ID{
				Type:     pkg.Raw,
				Name:     TestDep,
				Revision: revisionMap[TestDep],
			},
		},
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
