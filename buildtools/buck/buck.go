package buck

import (
	"fmt"
	"runtime"
	"strings"
	"sync"

	"github.com/apex/log"
	"github.com/remeh/sizedwaitgroup"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// AuditOutput models the output from the `buck audit` command.
type AuditOutput struct {
	OutputMapping map[string][]string
}

// Buck defines an interface for all Buck tool implementations.
type Buck interface {
	Deps(bool) (graph.Deps, error)
}

// Cmd implements Buck and defines how to retrieve buck output.
type Setup struct {
	Target string
	Cmd    func(string, ...string) (string, error)
}

// New creates a new Buck instance that calls the buck build tool directly.
func New(target string) Buck {
	return Setup{
		Target: target,
		Cmd:    Cmd,
	}
}

// Deps finds and uploads the dependencies of a Buck target using the supplied command and
// returns the dependency graph.
func (b Setup) Deps(upload bool) (graph.Deps, error) {
	locatorMap, err := uploadDeps(b, upload)
	if err != nil {
		return graph.Deps{}, nil
	}

	transDeps, err := depGraph(b, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	imports, err := directDeps(b, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}, nil
}

func uploadDeps(b Setup, upload bool) (map[string]fossa.Locator, error) {
	locatorMap := make(map[string]fossa.Locator)
	depList := AuditOutput{}
	var err error
	if true {
		depList, err = allSubprojectDeps(b)
		if err != nil {
			return locatorMap, err
		}
	} else {
		depList, err = cmdAudit(b.Cmd, "input", b.Target)
		if err != nil {
			return locatorMap, err
		}
	}

	rootDir, err := b.Cmd("root")
	if err != nil {
		return locatorMap, errors.Wrap(err, "Cannot get buck root")
	}
	rootDir = strings.TrimSpace(rootDir)

	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	lock := sync.RWMutex{}
	// Upload individual dependencies and keep a reference to the generated locators.
	for d, f := range depList.OutputMapping {
		wg.Add()
		go func(dep string, files []string) {
			defer wg.Done()
			locator, err := fossa.UploadTarballDependencyFiles(rootDir, files, sanitizeBuckTarget(dep), upload)
			fmt.Println(dep, sanitizeBuckTarget(dep))
			if err != nil {
				log.Warnf("Cannot upload files for %v: %s", dep, err)
			}

			lock.Lock()
			locatorMap[dep] = locator
			lock.Unlock()
		}(d, f)
	}
	wg.Wait()

	return locatorMap, nil
}

// allSubprojectDeps takes a target such as //third-party/... and determines
// the full list of dependencies specified by its targets.
func allSubprojectDeps(b Setup) (AuditOutput, error) {
	allInputs := AuditOutput{OutputMapping: make(map[string][]string)}

	targets, err := cmdTargets(b.Cmd, b.Target)
	if err != nil {
		return allInputs, err
	}
	targetInputLists := make(chan AuditOutput, len(targets))
	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	for _, target := range targets {
		wg.Add()
		go func(t string) {
			defer wg.Done()
			inputList, err := cmdAudit(b.Cmd, "input", t)
			targetInputLists <- inputList
			if err != nil {
				log.Warnf("Cannot retrieve inputs for %v: %s", t, err)
			}
		}(target)
	}
	wg.Wait()

	close(targetInputLists)
	for targetInputList := range targetInputLists {
		for name, values := range targetInputList.OutputMapping {
			_, present := allInputs.OutputMapping[name]
			if !present {
				allInputs.OutputMapping[name] = values
			}
		}
	}

	return allInputs, err
}

func depGraph(b Setup, locatorMap map[string]fossa.Locator) (map[pkg.ID]pkg.Package, error) {
	transitiveDeps := make(map[pkg.ID]pkg.Package)

	allDependencies := []string{}
	if true {
		// We do not need to check for the transitive graph because we assume the given
		// target returns all build rules underneath it, flattening the dependency graph.
		var err error
		allDependencies, err = cmdTargets(b.Cmd, b.Target)
		if err != nil {
			return transitiveDeps, err
		}
	} else {
		depList, err := cmdAudit(b.Cmd, "dependencies", b.Target, "--transitive")
		if err != nil {
			return transitiveDeps, err
		}
		allDependencies = depList.OutputMapping[b.Target]
	}

	mapLock := sync.RWMutex{}
	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	for _, d := range allDependencies {
		fmt.Println(d)
		wg.Add()
		go func(dep string) {
			defer wg.Done()
			transDeps, err := cmdAudit(b.Cmd, "dependencies", dep)
			if err != nil {
				log.Warnf("Cannot retrieve dependency list for %v: %s", dep, err)
			}

			var imports []pkg.Import
			for _, transDep := range transDeps.OutputMapping[dep] {
				fmt.Println(transDep, pkg.Raw, locatorMap[transDep].Project, locatorMap[transDep].Revision)
				imports = append(imports, pkg.Import{
					Target: transDep,
					Resolved: pkg.ID{
						Type:     pkg.Raw,
						Name:     locatorMap[transDep].Project,
						Revision: locatorMap[transDep].Revision,
					},
				})
			}
			id := pkg.ID{
				Type:     pkg.Raw,
				Name:     locatorMap[dep].Project,
				Revision: locatorMap[dep].Revision,
			}

			mapLock.Lock()
			transitiveDeps[id] = pkg.Package{
				ID:      id,
				Imports: imports,
			}
			mapLock.Unlock()
		}(d)
	}
	wg.Wait()

	return transitiveDeps, nil
}

func directDeps(b Setup, locatorMap map[string]fossa.Locator) ([]pkg.Import, error) {
	imports := []pkg.Import{}
	directDeps := []string{}
	if true {
		var err error
		directDeps, err = cmdTargets(b.Cmd, b.Target)
		if err != nil {
			return imports, err
		}
	} else {
		deps, err := cmdAudit(b.Cmd, "dependencies", b.Target)
		if err != nil {
			return imports, err
		}
		directDeps = deps.OutputMapping[b.Target]
	}
	for _, dep := range directDeps {
		imports = append(imports, pkg.Import{
			Target: dep,
			Resolved: pkg.ID{
				Type:     pkg.Raw,
				Name:     locatorMap[dep].Project,
				Revision: locatorMap[dep].Revision,
			},
		})
	}

	return imports, nil
}

// Change buildtarget `//src/fossa/buildtools:buck` into `buildtools-buck`
// to appease core and simplify naming.
func sanitizeBuckTarget(target string) string {
	depSplit := strings.Split(target, "/")
	return strings.Replace(depSplit[len(depSplit)-1], ":", "-", 1)
}
