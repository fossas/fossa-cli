package dotnet

import (
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/exec"
)

type DotNET struct {
	Cmd string
}

func (d *DotNET) Build(dir string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: d.Cmd,
		Argv: []string{"restore"},
		Dir:  dir,
	})
	return err
}

func (d *DotNET) References(projectFile string) ([]string, error) {
	stdout, _, err := exec.Run(exec.Cmd{
		Name: d.Cmd,
		Argv: []string{"list", projectFile, "reference"},
		WithEnv: map[string]string{
			"TERM": "dumb",
		},
	})
	if err != nil {
		return nil, err
	}

	header := "Project reference(s)\n--------------------\n"
	headerIndex := strings.Index(stdout, header)
	if headerIndex == -1 {
		return nil, nil
	}

	var projects []string
	contents := stdout[headerIndex+len(header):]
	for _, line := range strings.Split(contents, "\n") {
		if line == "" {
			break
		}
		project := filepath.Join(filepath.Dir(projectFile), Path(line))
		projects = append(projects, project)
	}

	return projects, nil
}

func Path(s string) string {
	return filepath.Join(strings.Split(s, "\\")...)
}
