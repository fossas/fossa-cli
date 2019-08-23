package rpm

import (
	"path/filepath"
	"strings"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

const licenseDirectory = "/usr/share/licenses"

// Shell holds information and the shell command for RPM analysis.
type Shell struct {
	LicenseDirectory string
	Upload           bool
	Cmd              func(...string) (string, error)
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
		Cmd: func(argv ...string) (string, error) {
			out, _, _ := exec.Run(exec.Cmd{
				Name: "rpm",
				Argv: argv,
			})
			return out, nil
		},
	}
}

func (s Shell) allPackagesCmd() (string, error) {
	output, err := s.Cmd("-qa", "--queryformat", "%{name},%{version},%{license}\n")
	if err != nil {
		return "", err
	}
	return output, nil
}

func (s Shell) singlePackageCmd(target string) (string, error) {
	output, err := s.Cmd("-q", "--queryformat", "%{name},%{version},%{license}\n", target)
	if err != nil {
		return "", err
	}
	return output, nil
}

// SinglePackage uploads license information pertaining to a single dependency and its dependencies.
func (s Shell) SinglePackage(target string) (graph.Deps, error) {
	output, err := s.singlePackageCmd(target)
	if err != nil {
		return graph.Deps{}, err
	}
	dep := dependencyFromOutput(output)

	locator, err := fossa.UploadTarballString(dep.name, dep.license, true, true, s.Upload)
	// TODO: Return a nice error
	if err != nil {
		log.Debugf("Error uploading %v: %+v", locator, err)
	}

	depGraph := graph.Deps{
		Transitive: make(map[pkg.ID]pkg.Package),
		Direct:     []pkg.Import{},
	}
	depID := pkg.ID{
		Type:     pkg.Raw,
		Name:     locator.Project,
		Revision: locator.Revision,
	}
	depGraph.Direct = append(depGraph.Direct, pkg.Import{
		Target:   dep.name,
		Resolved: depID,
	})

	depGraph.Transitive[depID] = pkg.Package{
		ID: depID,
	}
	return depGraph, nil
}

// SystemPackages uploads license information for all system level dependencies.
func (s Shell) SystemPackages() (graph.Deps, error) {
	output, err := s.allPackagesCmd()
	if err != nil {
		return graph.Deps{}, err
	}

	lines := strings.Split(output, "\n")
	dependencies := []dependency{}
	for _, line := range lines {
		dependencies = append(dependencies, dependencyFromOutput(line))
	}

	depGraph := graph.Deps{
		Transitive: map[pkg.ID]pkg.Package{},
	}

	//TODO: Concurrency
	for _, dep := range dependencies {
		// 1. Try to upload the copyright files in the system license directory. 70% are usually here.
		locator, err := fossa.UploadTarballDependency(filepath.Join(s.LicenseDirectory, dep.name), s.Upload, true)
		if err != nil {
			// 2. Upload the RPM declared license if the copyright files do not exist. This is normally always present.
			locator, err = fossa.UploadTarballString(dep.name, dep.license, true, true, s.Upload)
			if err != nil {
				log.Warnf("Error uploading %v: %+v", locator, err)
			}
		}

		depID := pkg.ID{
			Type:     pkg.Raw,
			Name:     locator.Project,
			Revision: locator.Revision,
		}
		depGraph.Transitive[depID] = pkg.Package{ID: depID}
		depGraph.Direct = append(depGraph.Direct, pkg.Import{Target: locator.Project, Resolved: depID})
	}

	return depGraph, nil
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
