package runfossa

import "github.com/fossas/fossa-cli/exec"

// Init executes fossa init the provided directory
func Init(projectDir string) (string, error) {
	output, err := runfossa(projectDir, []string{"init"})
	if err != nil {
		println(output)
		return output, err
	}

	return output, nil
}

var licenseReportPrefix = []string{"report", "licenses"}

func LicenseReport(projectDir string, args []string) (string, error) {
	args = append(licenseReportPrefix, args...)
	return runfossa(projectDir, args)
}

var dependencyReportPrefix = []string{"report", "dependencies"}

func DependencyReport(projectDir string, args []string) (string, error) {
	args = append(dependencyReportPrefix, args...)
	return runfossa(projectDir, args)
}

func runfossa(projectDir string, argv []string) (string, error) {
	cmd := exec.Cmd{
		Argv:    argv,
		Name:    "fossa",
		Dir:     projectDir,
		Command: "fossa",
	}

	stdout, stderr, err := exec.Run(cmd)
	if err != nil {
		return stderr, err
	}

	return stdout, err
}
