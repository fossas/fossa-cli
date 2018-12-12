package runfossa

import (
	"encoding/json"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/exec"
)

// Init executes fossa init the provided directory
func Init(projectDir string) (stdout, stderr string, err error) {
	return runfossa(projectDir, []string{"init"})
}

func LicenseReport(projectDir string, args []string) (stdout, stderr string, err error) {
	args = append([]string{"report", "licenses"}, args...)
	return runfossa(projectDir, args)
}

func DependencyReport(projectDir string, args []string) (stdout, stderr string, err error) {
	args = append([]string{"report", "dependencies"}, args...)
	return runfossa(projectDir, args)
}

func AnalyzeOutput(dir string, args []string) ([]fossa.SourceUnit, error) {
	stdout, _, err := runfossa(dir, append([]string{"analyze", "--output"}, args...))
	if err != nil {
		return nil, err
	}
	var parsed []fossa.SourceUnit
	err = json.Unmarshal([]byte(stdout), &parsed)
	if err != nil {
		return nil, err
	}
	return parsed, nil
}

func runfossa(projectDir string, argv []string) (stdout, stderr string, err error) {
	cmd := exec.Cmd{
		Argv:    argv,
		Name:    "fossa",
		Dir:     projectDir,
		Command: "fossa",
	}

	return exec.Run(cmd)

}
