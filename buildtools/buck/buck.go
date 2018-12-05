package buck

import (
	"encoding/json"
	"fmt"
	"runtime"
	"strings"
	"time"

	"github.com/apex/log"
	"github.com/remeh/sizedwaitgroup"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// AuditOutput models the output from "buck audit" command.
type AuditOutput struct {
	OutputMapping map[string][]string
}

// Buck defines an interface for all Buck tool implementations.
type Buck interface {
	Deps(bool) (graph.Deps, error)
}

// Cmd implements Buck and defines how to retrieve buck audit output.
type Cmd struct {
	RootDir func() (string, error)
	Target  string
	Audit   func(cmd, target string, args ...string) (AuditOutput, error)
}

// New creates a new Buck instance that calls the buck build tool.
func New(target string) Buck {
	return Cmd{
		RootDir: buckRoot,
		Target:  target,
		Audit:   buckAudit,
	}
}

// Deps finds and uploads the dependencies of a Buck target using the buck audit command and
// returns the dependency graph.
func (b Cmd) Deps(upload bool) (graph.Deps, error) {
	start := time.Now()
	locatorMap, err := uploadDeps(b, upload)
	if err != nil {
		return graph.Deps{}, nil
	}
	uploadTime := time.Now()
	transDeps, err := depGraph(b, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}

	imports, err := directDeps(b, locatorMap)
	if err != nil {
		return graph.Deps{}, nil
	}
	depsTime := time.Now()

	fmt.Println(uploadTime.Sub(start))
	fmt.Println(depsTime.Sub(uploadTime))

	return graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}, nil
}

func uploadDeps(b Cmd, upload bool) (map[string]fossa.Locator, error) {
	locatorMap := make(map[string]fossa.Locator)
	depList, err := b.Audit("input", b.Target)
	if err != nil {
		return locatorMap, err
	}

	rootDir, err := b.RootDir()
	if err != nil {
		return locatorMap, errors.Wrap(err, "Cannot get buck root")
	}

	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	// Upload individual dependencies and keep a reference to the generated locators.
	for d, f := range depList.OutputMapping {
		wg.Add()
		go func(dep string, files []string) {
			defer wg.Done()
			locator, err := fossa.UploadTarballDependencyFiles(rootDir, files, sanitizeBuckTarget(dep), upload)
			if err != nil {
				log.Warnf("Cannot make locator map: %s", err)
			}
			locatorMap[dep] = locator
		}(d, f)
	}
	wg.Wait()

	return locatorMap, nil
}

func depGraph(b Cmd, locatorMap map[string]fossa.Locator) (map[pkg.ID]pkg.Package, error) {
	transitiveDeps := make(map[pkg.ID]pkg.Package)
	depList, err := b.Audit("dependencies", b.Target, "--transitive")
	if err != nil {
		return transitiveDeps, err
	}

	wg := sizedwaitgroup.New(runtime.GOMAXPROCS(0))
	for _, d := range depList.OutputMapping[b.Target] {
		wg.Add()
		go func(dep string) {
			defer wg.Done()
			transDeps, err := b.Audit("dependencies", dep)
			if err != nil {
				log.Warn("o noooooo")
			}

			var imports []pkg.Import
			for _, transDep := range transDeps.OutputMapping[dep] {
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
			transitiveDeps[id] = pkg.Package{
				ID:      id,
				Imports: imports,
			}
		}(d)
	}
	wg.Wait()

	return transitiveDeps, nil
}

func directDeps(b Cmd, locatorMap map[string]fossa.Locator) ([]pkg.Import, error) {
	imports := []pkg.Import{}
	deps, err := b.Audit("dependencies", b.Target)
	if err != nil {
		return imports, err
	}

	for _, dep := range deps.OutputMapping[b.Target] {
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

// Change buildtarget "//src/fossa/buildtools:buck" into "buildtools-buck"
// to appease core and simplify naming.
func sanitizeBuckTarget(target string) string {
	depSplit := strings.Split(target, "/")
	return strings.Replace(depSplit[len(depSplit)-1], ":", "-", 1)
}

func buckAudit(cmd, target string, args ...string) (AuditOutput, error) {
	var output AuditOutput
	out, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: append([]string{"audit", cmd, "--json", target}, args...),
	})
	if err != nil {
		return output, errors.Wrapf(err, "Could not run `buck audit %s --json %s %+v` within the current directory", cmd, target, args)
	}

	err = json.Unmarshal([]byte(out), &output.OutputMapping)
	if err != nil {
		return output, errors.Wrap(err, "Could not unmarshal JSON into dependency list")
	}

	return output, nil
}

func buckRoot() (string, error) {
	root, _, err := exec.Run(exec.Cmd{
		Name: "buck",
		Argv: []string{"root"},
	})
	if err != nil {
		return root, errors.Wrap(err, "Could not run `buck root` within the current directory")
	}

	return strings.TrimSpace(root), nil
}
