package debian

import (
	"path/filepath"
	"sync"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Cmd struct {
	Directory string
	Upload    bool
	DebCmd    func(...string) (string, error)
}

// New returns a new Cmd that implements "apt-cache" to find dependency information.
func New() Cmd {
	return Cmd{
		Directory: "/usr/share/doc/",
		Upload:    true,
		DebCmd:    aptCache,
	}
}

// Dependencies returns a dependency graph by analyzing a debian package.Dependencies.
// Notes:
// 	(A "valid dependency" is one that has a valid path in "d.Directory+target"). Many types of
//	packages are found when running d.DebCmd and it is important to determine which ones
//	are used by the system. This is the reason we debug errors when a tarball upload fails.
//
// Analysis steps are as follows:
//	1. Find all transitive dependencies for the target package/.
//	2. Upload all valid dependencies and retrieve locator information.
//	3. Loop through all valid dependencies and find their direct dependencies using d.DebCmd
//	4. Construct a dependency graph with the direct dependency information.Dependencies
//
func (d Cmd) Dependencies(target string) (graph.Deps, error) {
	// Get all relevant dependencies.
	depList, err := transitiveDeps(d.DebCmd, target)
	if err != nil {
		return graph.Deps{}, err
	}

	// Loop over the dep list create the locator map from valid package uploads.
	locatorMap := uploadDeps(depList, d.Directory, d.Upload)

	// Loop over the locator map, get direct deps for each and build the graph.
	depGraph := dependencyGraph(d.DebCmd, locatorMap)

	directDeps := []pkg.Import{
		pkg.Import{
			Target: target,
			Resolved: pkg.ID{
				Type:     pkg.Raw,
				Name:     target,
				Revision: locatorMap[target],
			},
		},
	}

	return graph.Deps{
		Direct:     directDeps,
		Transitive: depGraph,
	}, nil
}

func uploadDeps(dependencies []string, directory string, upload bool) map[string]string {
	var locatorMap = make(map[string]string)
	for _, dep := range dependencies {
		if _, ok := locatorMap[dep]; !ok {
			revision, err := fossa.UploadTarballDependency(filepath.Join(directory, dep), upload)
			if err != nil {
				log.Debugf("Error uploading %v: %+v", dep, err)
			} else {
				locatorMap[dep] = revision.Revision
			}
		}
	}

	return locatorMap
}

func dependencyGraph(command func(...string) (string, error), locatorMap map[string]string) map[pkg.ID]pkg.Package {
	wg := sync.WaitGroup{}
	var depGraph = make(map[pkg.ID]pkg.Package)
	for t, r := range locatorMap {
		wg.Add(1)
		go func(target, revision string) {
			defer wg.Done()
			if _, ok := locatorMap[target]; ok {
				dependencies, err := directDeps(command, target)
				if err != nil {
					log.Debugf("Error retrieving deps for %+v: %+v", target, err)
				}

				importedDeps := []pkg.Import{}
				for _, dep := range dependencies {
					if _, ok := locatorMap[dep]; ok {
						importedDeps = append(importedDeps, pkg.Import{
							Target: dep,
							Resolved: pkg.ID{
								Type:     pkg.Raw,
								Name:     dep,
								Revision: locatorMap[dep],
							},
						})
					}
				}

				targetID := pkg.ID{
					Type:     pkg.Raw,
					Name:     target,
					Revision: revision,
				}
				depGraph[targetID] = pkg.Package{
					ID:      targetID,
					Imports: importedDeps,
				}
			}
		}(t, r)
	}
	wg.Wait()

	return depGraph
}
