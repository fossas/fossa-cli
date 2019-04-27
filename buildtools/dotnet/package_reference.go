package dotnet

import (
	"path/filepath"
	"regexp"

	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

type Manifest struct {
	PropertyGroup []PropertyGroup
	ItemGroup     []ItemGroup
}

type PropertyGroup struct {
	RootNamespace string
	Version       string
}

type ItemGroup struct {
	Reference        []Reference
	PackageReference []Reference
	ProjectReference []Reference
}

type Reference struct {
	Include string `xml:",attr"`
	Version string `xml:"Version"`
}

func (m *Manifest) Name() string {
	for _, propertyGroup := range m.PropertyGroup {
		if propertyGroup.RootNamespace != "" {
			return propertyGroup.RootNamespace
		}
	}
	return ""
}

func (m *Manifest) Version() string {
	for _, propertyGroup := range m.PropertyGroup {
		if propertyGroup.Version != "" {
			return propertyGroup.Version
		}
	}
	return ""
}

func (m *Manifest) packages() []Reference {
	var references []Reference
	for _, itemGroup := range m.ItemGroup {
		references = append(references, itemGroup.PackageReference...)
	}
	return references
}

func (m *Manifest) projects() []Reference {
	var projects []Reference
	for _, itemGroup := range m.ItemGroup {
		projects = append(projects, itemGroup.ProjectReference...)
	}
	return projects
}

// IsPackageReferenceFile checks for a valid package reference file name.
func IsPackageReferenceFile(filename string) bool {
	var xmlProj = regexp.MustCompile(`\.(cs|x|vb|db|fs)proj$`)
	return xmlProj.MatchString(filename)
}

// PackageReferenceGraph reads a package reference file and returns a dependency graph.
func PackageReferenceGraph(file string) (graph.Deps, error) {
	imports := []pkg.Import{}
	dependencyMap := make(map[pkg.ID]pkg.Package)

	projects := make(map[string]Manifest)
	err := Projects(projects, file)
	if err != nil {
		return graph.Deps{}, nil
	}

	for _, project := range projects {
		for _, reference := range project.packages() {
			id := pkg.ID{
				Type:     pkg.NuGet,
				Name:     reference.Include,
				Revision: reference.Version,
			}
			imports = append(imports, pkg.Import{
				Target:   reference.Include,
				Resolved: id,
			})
			dependencyMap[id] = pkg.Package{
				ID: id,
			}
		}
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: dependencyMap,
	}, nil
}

// Projects recursively discovers references to other manifest files from the base manifest file.
func Projects(projects map[string]Manifest, projectFile string) error {
	// Break out of cycles.
	if _, ok := projects[projectFile]; ok {
		return nil
	}

	var manifest Manifest
	err := files.ReadXML(&manifest, projectFile)
	if err != nil {
		return err
	}
	projects[projectFile] = manifest

	// Get all project reference files and recurse.
	for _, reference := range manifest.projects() {
		err = Projects(projects, filepath.Join(filepath.Dir(projectFile), Path(reference.Include)))
		if err != nil {
			return err
		}
	}

	return nil
}
