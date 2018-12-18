package debian

import (
	"strings"

	"github.com/fossas/fossa-cli/exec"
)

// Return all direct dependencies as an array of strings.
// Currently we ignore all virtual dependencies but include "Suggested" deps
// which are later verified before inclusion in the dep graph.
func directDeps(command func(...string) (string, error), target string) ([]string, error) {
	output, err := command(target)
	if err != nil {
		return nil, err
	}
	dependencies := []string{}
	lines := strings.Split(output, "\n")
	for _, line := range lines {
		dep := strings.Split(strings.Replace(line, " ", "", -1), ":")
		if len(dep) > 1 && !strings.HasPrefix(dep[1], "<") {
			dependencies = append(dependencies, dep[1])
		}
	}
	return dependencies, nil
}

// Return all transitive dependencies as an array of strings.
// Ignore all virtual dependencies and it is important to note that this
// can return duplicates.
func transitiveDeps(command func(...string) (string, error), target string) ([]string, error) {
	output, err := command("--recurse", target)
	if err != nil {
		return nil, err
	}

	dependencies := []string{}
	lines := strings.Split(output, "\n")
	for _, line := range lines {
		dep := strings.Split(strings.Replace(line, " ", "", -1), ":")
		if len(dep) == 1 && dep[0] != "" && !strings.HasPrefix(dep[0], "<") {
			dependencies = append(dependencies, dep[0])
		}
	}
	return dependencies, nil
}

// Return the string output from running "apt-cache" with the supplied arguments.
func aptCache(argv ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "apt-cache",
		Argv: append([]string{"depends"}, argv...),
	})
	if err != nil {
		return "", nil
	}
	return out, nil
}
