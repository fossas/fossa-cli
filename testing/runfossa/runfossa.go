package runfossa

import "github.com/fossas/fossa-cli/exec"

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

func runfossa(projectDir string, argv []string) (stdout, stderr string, err error) {
	cmd := exec.Cmd{
		Argv:    argv,
		Name:    "fossa",
		Dir:     projectDir,
		Command: "fossa",
	}

	return exec.Run(cmd)

}
