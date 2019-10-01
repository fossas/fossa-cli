package gradle

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

func Deps(dir, target module.Filepath) (graph.Deps, *errors.Error) {
	// Get the correct gradle command to run from the directory
	cmd, err := GradleCmd(dir)
	if err != nil {
		return graph.Deps{}, err
	}

	input := NewShellInput(cmd, dir, true, "", 0)
	return DepsWithCommand(input, dir, target)
}

func DepsWithCommand(input Input, dir, target string) (graph.Deps, *errors.Error) {
	// Run gradle tasks on the target to get all dependency tasks
	projects, err := input.DependencyTasks()
	if err != nil {
		return graph.Deps{}, errors.UnknownError(err, "")
	}

	depMap := map[pkg.ID]pkg.Package{}
	directSet := map[pkg.Import]bool{}
	for _, project := range projects {
		fmt.Println(project)
		depGraph, err := Dependencies(project, input)
		if err != nil {
			return graph.Deps{}, errors.UnknownError(err, "")
		}

		projectDirectSet := map[pkg.Import]bool{}
		for configuration, configGraph := range depGraph {
			for _, dep := range configGraph.Direct {
				projectDirectSet[dep] = true
				directSet[dep] = true
			}

			// Look through each dep, set imports if it is larger and add the current usage.
			for dep, pack := range configGraph.Transitive {
				currDep := depMap[dep]

				currDep.ID = dep
				if currDep.Usage == nil {
					currDep.Usage = map[string]bool{}
				}
				currDep.Usage[configuration] = true

				// Hack
				if len(pack.Imports) > len(currDep.Imports) {
					currDep.Imports = pack.Imports
				}

				depMap[dep] = currDep
			}
		}

		projectDirectList := []pkg.Import{}
		for projectDep := range projectDirectSet {
			projectDirectList = append(projectDirectList, projectDep)
		}

		projectID := pkg.ID{Type: pkg.Gradle, Name: project}
		depMap[projectID] = pkg.Package{
			ID:      projectID,
			Imports: projectDirectList,
			Usage:   depMap[projectID].Usage,
		}
	}

	directList := []pkg.Import{}
	for dep := range directSet {
		directList = append(directList, dep)
	}

	return graph.Deps{
		Direct:     directList,
		Transitive: depMap,
	}, nil
}

// GradleCmd finds the best possible gradle command to run for
// shell commands.
func GradleCmd(dir string) (string, *errors.Error) {
	cmd, _, err := exec.Which("--version", os.Getenv("FOSSA_GRADLE_CMD"), filepath.Join(dir, "gradlew"), filepath.Join(dir, "gradlew.bat"), "gradle")
	if err != nil {
		return "", &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: "FOSSA could not find a valid gradle binary to run. This will prevent your gradle projects from being accurately analyzed",
		}
	}

	return cmd, nil
}
