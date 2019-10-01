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

// Deps finds the correct gradle command to use and pieces together
// an accurate dependency graoh of the entire gradle project.
func Deps(dir, target module.Filepath) (graph.Deps, *errors.Error) {
	cmd, err := bestCmd(dir)
	if err != nil {
		return graph.Deps{}, err
	}

	input := NewShellInput(cmd, dir, true, "", 0)
	return DepsWithCommand(input, dir, target)
}

// DepsWithCommand uses the supplied command to find an accurate
// dependency graph.
func DepsWithCommand(input Input, dir, target string) (graph.Deps, *errors.Error) {
	projects, err := input.DependencyTasks()
	if err != nil {
		return graph.Deps{}, &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: "`gradle tasks --all` was unable to be run. This likely represents an issue with your gradle configuration and should be diagnosed by attempting to the run the command and fix any associated errors. FOSSA will be unable to accurately determine your dependency graph until this issue is fixed.",
		}
	}

	depMap := map[pkg.ID]pkg.Package{}
	directSet := map[pkg.Import]bool{}
	for _, project := range projects {
		depGraph, err := Dependencies(project, input)
		if err != nil {
			return graph.Deps{}, &errors.Error{
				Cause:           err,
				Type:            errors.Exec,
				Troubleshooting: fmt.Sprintf("`gradle %s:dependencies` was unable to be run. This likely signifies an issue with your gradle build and should be diagnosed by attempting to the run the command and fixing any associated errors. FOSSA will be unable to accurately determine your dependency graph until this issue is fixed.", project),
			}
		}

		projectDirectSet := map[pkg.Import]bool{}
		for configuration, configGraph := range depGraph {
			// Collect each sub-project's direct dependencies and
			// combine to create the whole project's direct dependencies.
			for _, dep := range configGraph.Direct {
				projectDirectSet[dep] = true
				directSet[dep] = true
			}

			// Look through each dep and set id, usage, and imports.
			for depID, depPackage := range configGraph.Transitive {
				currentDep := depMap[depID]
				currentDep.ID = depID

				if currentDep.Usage == nil {
					currentDep.Usage = map[string]bool{}
				}
				currentDep.Usage[configuration] = true

				// We prefer the larger list for two reasons:
				// 1. With sub-projects, the direct dependencies are
				// 	determined on a project level and not here.
				// 2. With packages, the list of dependencies will be
				// 	identical across sub-projects, unless repeated within
				// 	a single sub-project, in which case there will be 0 deps.
				if len(depPackage.Imports) > len(currentDep.Imports) {
					currentDep.Imports = depPackage.Imports
				}

				depMap[depID] = currentDep
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

// bestCmd finds the best possible gradle command to run for shell commands.
func bestCmd(dir string) (string, *errors.Error) {
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
