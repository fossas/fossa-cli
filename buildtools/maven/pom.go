package maven

import (
	"encoding/xml"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// A Manifest represents a POM manifest file.
type Manifest struct {
	Project              xml.Name     `xml:"project"`
	Parent               Parent       `xml:"parent"`
	Modules              []string     `xml:"modules>module"`
	ArtifactID           string       `xml:"artifactId"`
	GroupID              string       `xml:"groupId"`
	Version              string       `xml:"version"`
	Description          string       `xml:"description"`
	Name                 string       `xml:"name"`
	URL                  string       `xml:"url"`
	Dependencies         []Dependency `xml:"dependencies>dependency"`
	DependencyManagement []Dependency `xml:"dependencyManagement>dependencies>dependency"`
}

type Parent struct {
	ArtifactID string `xml:"artifactId"`
	GroupID    string `xml:"groupId"`
	Version    string `xml:"version"`
}

type Dependency struct {
	GroupId    string `xml:"groupId"`
	ArtifactId string `xml:"artifactId"`
	Version    string `xml:"version"`

	// Scope is where the dependency is used, such as "test" or "runtime".
	Scope string `xml:"scope"`

	Failed bool
}

// ID returns the dependency identifier as groupId:artifactId.
func (d Dependency) ID() string {
	return d.GroupId + ":" + d.ArtifactId
}

// PomFileGraph returns simply the list of dependencies listed within the manifest file.
func PomFileGraph(target, dir string) (graph.Deps, error) {
	pom, err := ResolveManifestFromTarget(target, dir)
	if err != nil {
		return graph.Deps{}, err
	}

	// Aggregate `dependencies` and `dependencyManagement` fields.
	dependencyList := combineDependencies(pom.Dependencies, pom.DependencyManagement)

	deps := graph.Deps{
		Direct:     depsListToImports(dependencyList),
		Transitive: make(map[pkg.ID]pkg.Package),
	}

	// From just a POM file we don't know what really depends on what, so list all imports in the graph.
	for _, dep := range dependencyList {
		pack := pkg.Package{
			ID: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.ID(),
				Revision: dep.Version,
			},
		}
		deps.Transitive[pack.ID] = pack
	}

	return deps, nil
}

// ResolveManifestFromTarget tries to determine what target is supposed to be and then reads the POM
// manifest file pointed to by target if it is a path to such a file or module.
func ResolveManifestFromTarget(target, dir string) (*Manifest, error) {
	pomFile := filepath.Join(dir, target)
	stat, err := os.Stat(pomFile)
	if err != nil {
		// target is not a path.
		if strings.Count(target, ":") == 1 {
			// This is likely a module ID.
			return nil, errors.Errorf("cannot identify POM file for module %q", target)
		}
		return nil, errors.Wrapf(err, "manifest file for %q cannot be read", target)
	}
	if stat.IsDir() {
		// We have the directory and will assume it uses the standard name for the manifest file.
		pomFile = filepath.Join(target, "pom.xml")
	}

	var pom Manifest
	if err := files.ReadXML(&pom, pomFile); err != nil {
		return nil, err
	}
	return &pom, nil
}

// combineDependencies combines and dedupes two lists of Dependencies.
func combineDependencies(listOne, listTwo []Dependency) []Dependency {
	mergedList := listOne
	listOneMap := make(map[Dependency]bool)
	for _, dep := range listOne {
		listOneMap[dep] = true
	}
	for _, dep := range listTwo {
		if _, exists := listOneMap[dep]; !exists {
			mergedList = append(mergedList, dep)
		}
	}
	return mergedList
}
