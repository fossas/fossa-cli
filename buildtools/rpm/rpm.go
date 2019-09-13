package rpm

// RPM Analysis analyzes system level RPMs
// and single RPMs, butlicense information
// is obtained the same way for both.
//
// License Information can be obtained in two different ways.
// 1. The output of running rpm with `--queryformat %{name},%{version},%{license}\n`
// 	returns the license information that the maintainer of
// 	each package has set (Declared License).
// 2. Installed packages store their copyright information in
// 	a licenses directory, usually at `usr/share/licenses`.
// 	This information is significantly more accurate as it
//	includes full license files. This method is preferred
//	to finding declared license information.
//
// System Level: Attempt to analyze all installed RPM packages.
// 1. Run `rpm -qa --queryformat %{name},%{version},%{license}\n`
// 	To return information about every installed RPM.
// 2. Find the correct license for each dependency.
// 3. Upload the license and retrieve its fossa locator.
// 4. Associate these locators with the project and construct the graph.
//
// Single Package:
// 1. Run `yum install RPM` to install the dependency. This is
// 	not mandatory but will provide better license information.
// 2. Run `rpm -qR RPM` get the dependency's required dependencies.
// 3. Recursively run steps 1 and 2 until all dependencies have been installed.
// 4. Find the correct license for each dependency.
// 5. Upload license information for each dependency and save the locator.
// 6. Construct the dependency graph using the saved locators which are stored
//	in a map that has a reference to their dependency name.

import (
	"fmt"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"github.com/apex/log"
	"github.com/remeh/sizedwaitgroup"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

const licenseDirectory = "/usr/share/licenses"

// Shell holds information and the shell command for RPM analysis.
type Shell struct {
	LicenseDirectory string
	Upload           bool
	RPM              func(argv ...string) (stdout string, stderr string, err error)
	Yum              func(argv ...string) (stdout string, stderr string, err error)
}

type dependency struct {
	name     string
	revision string
	license  string
}

// ShellOutput creates a Shell which shells out to rpm to generate values to return.
func ShellOutput() Shell {
	return Shell{
		LicenseDirectory: licenseDirectory,
		Upload:           true,
		RPM: func(argv ...string) (string, string, error) {
			out, stdout, err := exec.Run(exec.Cmd{Name: "rpm", Argv: argv})
			return out, stdout, err
		},
		Yum: func(argv ...string) (string, string, error) {
			out, stdout, err := exec.Run(exec.Cmd{Name: "yum", Argv: argv})
			return out, stdout, err
		},
	}
}

func (s Shell) allPackagesRPM() (string, *errors.Error) {
	arguments := []string{"-qa", "--queryformat", "%{name},%{version},%{license}\n"}
	stdout, stderr, err := s.RPM(arguments...)
	if err != nil {
		return "", &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("The command rpm %+v could not be run and information about the systems dependencies could not be retrieved. Try running this command on your own and ensure that RPM is installed before filing a bug.\nstderr: %s\nstdout: %s", arguments, stderr, stdout),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/rpm.md#rpm",
		}
	}
	return stdout, nil
}

func (s Shell) singlePackageRPM(target string) (string, *errors.Error) {
	arguments := []string{"-q", "--queryformat", "%{name},%{version},%{license}\n", target}
	stdout, stderr, err := s.RPM(arguments...)
	if err != nil {
		return "", &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("The command rpm %+v could not be run and information about %s could not be retrieved. Try running this command on your own, if that fails try to first install %s.\nstderr: %s\nstdout: %s", arguments, target, target, stderr, stdout),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/rpm.md#rpm",
		}
	}
	return stdout, nil
}

func (s Shell) providers(target string) []string {
	arguments := []string{"-q", "--whatprovides", target}
	stdout, _, err := s.RPM(arguments...)
	if err != nil {
		return []string{}
	}
	providers := []string{}
	lines := strings.Split(stdout, "\n")
	for _, line := range lines {
		if strings.HasPrefix(line, "no package provides") || line == "" || strings.Contains(line, "not installed") {
			continue
		}
		providers = append(providers, line)
	}

	if len(providers) == 0 {
		providers = []string{target}
	}

	return providers
}

func (s Shell) transitiveDepsRPM(target string) (string, *errors.Error) {
	arguments := []string{"-qR", target}
	stdout, stderr, err := s.RPM(arguments...)
	if err != nil {
		return "", &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("The command rpm %+v could not be run and information about %s's dependencies could not be retrieved. Try running this command on your own, if that fails try to first install %s.\nstderr: %s\nstdout: %s", arguments, target, target, stderr, stdout),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/rpm.md#rpm",
		}
	}
	return stdout, nil
}

func (s Shell) yumInstall(target string) *errors.Error {
	arguments := []string{"install", target, "-y"}
	stdout, stderr, err := s.Yum(arguments...)
	if err != nil {
		return &errors.Error{
			Cause:           err,
			Type:            errors.Exec,
			Troubleshooting: fmt.Sprintf("The command yum %+v could not be run and %s could not be installed.\nstderr: %s\nstdout: %s", arguments, target, stderr, stdout),
			Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/integrations/rpm.md#rpm",
			Message:         fmt.Sprintf("This may not cause any issues but could prevent accurate dependency and license information from being found. If you believe that %s does not need to be installed and accurate information has been found please ignore this error.", target),
		}
	}
	return nil
}

// Parse rpm output for package names. Packages can be listed
// multiple times, so we store the information in a map so that
// we do not need to dedupe a list at the end.
func parseTransitive(output string) []string {
	uniqueDependencies := make(map[string]bool)
	lines := strings.Split(output, "\n")
	for _, line := range lines {
		dep := strings.Split(line, " ")[0]
		depName := strings.Split(dep, "(")[0]
		if depName == "" || strings.Contains(depName, "warning") {
			continue
		}
		if depName == "rpmlib" {
			depName = "rpm"
		}
		uniqueDependencies[depName] = true
	}

	dependencies := make([]string, 0, len(uniqueDependencies))
	for key := range uniqueDependencies {
		dependencies = append(dependencies, key)
	}

	return dependencies
}

// Recursively attempt to install dependencies. If the dependency has already been found then it is skipped.
func recursiveInstall(target string, s Shell, dependencyImports map[string][]string) {
	if _, ok := dependencyImports[target]; ok {
		return
	}

	err := s.yumInstall(target)
	if err != nil {
		log.Warn(err.Error())
	}

	fullDependencyList := ""
	providers := s.providers(target)
	for _, provider := range providers {
		output, err := s.transitiveDepsRPM(provider)
		if err != nil {
			log.Warn(err.Error())
			dependencyImports[target] = []string{}
			return
		}
		fullDependencyList = fullDependencyList + output
	}

	dependencyImports[target] = parseTransitive(fullDependencyList)
	for _, dep := range dependencyImports[target] {
		recursiveInstall(dep, s, dependencyImports)
	}
}

// SinglePackage uploads license information pertaining to a single dependency and its dependencies.
// This function maintains a dependencyMap which references dependency names to their transitive
// dependencies and an IDMap which references a dependency to its fossa ID.
func (s Shell) SinglePackage(target string) (graph.Deps, error) {
	IDMap := make(map[string]pkg.ID)
	depMap := make(map[string][]string)
	recursiveInstall(target, s, depMap)

	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	mapLock := sync.RWMutex{}
	// Retrieve version and license guess for each dependency.
	for d := range depMap {
		wg.Add()
		go func(dep string) {
			defer wg.Done()

			providers := s.providers(dep)
			if len(providers) == 0 {
				providers = []string{dep}
			}
			output, err := s.singlePackageRPM(providers[0])
			if err != nil {
				log.Warn(err.Error())
			}

			parsedDependency := dependencyFromOutput(output)
			locator, err := findAndUploadLicense(parsedDependency, s.LicenseDirectory, s.Upload)
			if err != nil {
				log.Warn(err.Error())
			}

			mapLock.Lock()
			IDMap[dep] = pkg.ID{
				Type:     pkg.Raw,
				Name:     locator.Project,
				Revision: locator.Revision,
			}
			mapLock.Unlock()
		}(d)
	}
	wg.Wait()

	depGraph := graph.Deps{
		Direct:     []pkg.Import{pkg.Import{Target: target, Resolved: IDMap[target]}},
		Transitive: make(map[pkg.ID]pkg.Package),
	}

	for _, id := range IDMap {
		imports := []pkg.Import{}

		for _, dep := range depMap[id.Name] {
			imports = append(imports, pkg.Import{
				Target:   dep,
				Resolved: IDMap[dep],
			})
		}

		depGraph.Transitive[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	return depGraph, nil
}

// SystemPackages uploads license information for all system level dependencies.
func (s Shell) SystemPackages() (graph.Deps, error) {
	output, err := s.allPackagesRPM()
	if err != nil {
		return graph.Deps{}, err
	}

	lines := strings.Split(output, "\n")
	dependencies := []dependency{}
	for _, line := range lines {
		dependencies = append(dependencies, dependencyFromOutput(line))
	}

	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	mapLock := sync.RWMutex{}
	depGraph := graph.Deps{
		Transitive: map[pkg.ID]pkg.Package{},
	}
	for _, d := range dependencies {
		wg.Add()
		go func(dep dependency) {
			defer wg.Done()

			locator, err := findAndUploadLicense(dep, s.LicenseDirectory, s.Upload)
			if err != nil {
				log.Warn(err.Error())
			}

			depID := pkg.ID{
				Type:     pkg.Raw,
				Name:     locator.Project,
				Revision: locator.Revision,
			}

			mapLock.Lock()
			depGraph.Transitive[depID] = pkg.Package{ID: depID}
			depGraph.Direct = append(depGraph.Direct, pkg.Import{Target: locator.Project, Resolved: depID})
			mapLock.Unlock()
		}(d)
	}
	wg.Wait()

	return depGraph, nil
}

func findAndUploadLicense(dep dependency, licenseDir string, upload bool) (fossa.Locator, *errors.Error) {
	// 1. Try to upload the copyright files in the system license directory. 70% are usually here.
	locator, err := fossa.UploadTarballDependency(filepath.Join(licenseDir, dep.name), upload, true)
	if err != nil {
		// 2. Upload the RPM declared license if the copyright files do not exist. This is normally always present.
		var uploadErr error
		locator, uploadErr = fossa.UploadTarballString(dep.name, dep.license, true, true, upload)
		if uploadErr != nil {
			return locator, &errors.Error{
				Cause:           uploadErr,
				Type:            errors.Unknown,
				Troubleshooting: fmt.Sprintf("There was a problem uploading license information for %s. Ensure that you have a working network connection and that you can reach app.fossa.com.", dep.name),
			}
		}
		log.Warnf("license files for `%s` could not be found in `%s`. The declared license `%s` will be used: %s", dep.name, licenseDir, dep.license, err.Error())
	}

	return locator, nil
}

// Parse dependency from output format dep-name,version,license.
func dependencyFromOutput(line string) dependency {
	splitLine := strings.Split(line, ",")
	newDep := dependency{}
	if len(splitLine) > 0 {
		newDep.name = splitLine[0]
	}
	if len(splitLine) > 1 {
		newDep.revision = splitLine[1]
	}
	if len(splitLine) > 2 {
		newDep.license = splitLine[2]
	}

	return newDep
}
