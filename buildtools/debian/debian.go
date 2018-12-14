package debian

import (
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type AptOut struct {
	dependencies map[string][]string
}

type Cmd struct {
	NoUpload bool
	DebCmd   func(...string) (string, error)
}

func New() Cmd {
	return Cmd{
		NoUpload: false,
		DebCmd:   DebCommand,
	}
}

// Get a list of all dependencies from apt-cache depends PACKAGE. Depends, Suggests, and Recomends.
// A Valid package is one with a folder inside of doc. Check if it exists before adding to map and searching for deps.
// Packages can have circular dependencies so make sure we check for existence in the map
// Ignore virtual packages right now and figure out how to handle them after basic functionality works.
func (d Cmd) Dependencies(target string) (graph.Deps, error) {
	// Get all relevant dependencies.
	depList, err := TransitiveDeps(target)
	if err != nil {
		return graph.Deps{}, err
	}
	// Loop over the dep list create the locator map from valid package uploads.
	locatorMap := uploadDeps(depList)

	// Loop over the locator map, get direct deps for each and build the graph.
	depGraph := dependencyGraph(locatorMap)

	directDeps := []pkg.Import{
		pkg.Import{
			Target: target,
			Resolved: pkg.ID{
				Type:     pkg.Debian,
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

func uploadDeps(dependencies []string) map[string]string {
	var locatorMap = make(map[string]string)
	dir := "/usr/share/doc/"
	for _, dep := range dependencies {
		if _, ok := locatorMap[dep]; !ok {
			revision, err := fossa.UploadTarballDependency(filepath.Join(dir, dep))
			if err != nil {
				log.Debugf("Error uploading %v: %+v", dep, err)
			} else {
				locatorMap[dep] = revision.Revision
			}
		}
	}

	return locatorMap
}

func dependencyGraph(locatorMap map[string]string) map[pkg.ID]pkg.Package {
	var depGraph = make(map[pkg.ID]pkg.Package)

	for target, revision := range locatorMap {
		dependecies, err := DirectDeps(target)
		if err != nil {
			log.Warnf("New error: %+v", err)
		}

		importedDeps := []pkg.Import{}
		for _, dep := range dependecies {
			if _, ok := locatorMap[dep]; !ok {
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

	return depGraph
}
