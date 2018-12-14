package debian

import (
	"strings"

	"github.com/fossas/fossa-cli/exec"
)

// Parse the dependencies, don't include virtual dependencies.
func DirectDeps(target string) ([]string, error) {
	output, err := DebCommand(target)
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

// Parse the dependencies, don't include virtual dependencies.
func TransitiveDeps(target string) ([]string, error) {
	output, err := DebCommand("--recurse", target)
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

func DebCommand(argv ...string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "apt-cache",
		Argv: append([]string{"depends"}, argv...),
	})
	if err != nil {
		return "", nil
	}
	return out, nil
}
