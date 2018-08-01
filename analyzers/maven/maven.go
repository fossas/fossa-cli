// Package maven implements Maven analysis.
//
// A `BuildTarget` for Maven is a reactor build target
// specified by [groupId]:artifactId or by its relative path
package maven

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Maven   maven.Maven
	Options Options
}

type Options struct {
	Binary  string `mapstructure:"bin"`
	Command string `mapstructure:"cmd"`
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debugf("%#v", opts)

	// Decode options.
	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	// Get Maven binary.
	mvnBin, _, err := exec.Which("--version", options.Binary, os.Getenv("MAVEN_BINARY"), "mvn")

	analyzer := Analyzer{
		Maven: maven.Maven{
			Cmd: mvnBin,
		},
		Options: options,
	}

	log.Logger.Debugf("%#v", analyzer)
	return &analyzer, nil
}

func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	log.Logger.Debugf("%#v", dir)
	var modules []module.Module

	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		if info.IsDir() {
			ok, err := files.Exists(path, "pom.xml")
			if err != nil {
				return err
			}
			if !ok {
				return nil
			}

			dir := filepath.Dir(path)
			submodules, err := a.Maven.Modules(dir)
			if len(submodules) == 0 {
				// Failed to parse modules via calling into `mvn` or this is a single-module build
				// Fallback to parsing the pom.xml and skipping the underlying file tree assuming they are subprojects
				if err != nil {
					log.Logger.Debugf("Unable to parse modules through `mvn`: %#v %#v", err.Error(), err)
				}
				var parsedPom maven.Manifest
				pomParseError := files.ReadXML(&parsedPom, path, "pom.xml")

				groupId := parsedPom.GroupID
				if groupId == "" {
					groupId = parsedPom.Parent.GroupID
				}

				if parsedPom.Name == "" || groupId == "" || parsedPom.ArtifactID == "" {
					pomParseError = errors.Errorf("invalid manifest at %s/pom.xml", path)
				}

				if pomParseError != nil {
					log.Logger.Warningf("Unable to parse module: %#v %#v", pomParseError.Error(), pomParseError)
					return nil
				}

				modules = append(modules, module.Module{
					Name:        parsedPom.Name,
					Type:        pkg.Maven,
					BuildTarget: groupId + ":" + parsedPom.ArtifactID,
					Dir:         dir,
				})
			} else {
				for _, m := range submodules {
					modules = append(modules, module.Module{
						Name:        m,
						Type:        pkg.Maven,
						BuildTarget: m,
						Dir:         dir,
					})
				}
			}

			log.Logger.Debugf("skipDir: %#v", path)
			// Don't continue recursing, because anything else is probably a
			// subproject.
			return filepath.SkipDir
		}

		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find Maven projects")
	}

	return modules, nil
}

func (a *Analyzer) Clean(m module.Module) error {
	return a.Maven.Clean(m.Dir)
}

func (a *Analyzer) Build(m module.Module) error {
	return a.Maven.Compile(m.Dir)
}

// IsBuilt checks whether `mvn dependency:list` produces an error.
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	output, err := a.Maven.DependencyList(m.Dir)
	if err != nil {
		if strings.Contains(output, "Could not find artifact") {
			return false, nil
		}
		return false, err
	}
	return output != "", nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("%#v", m)

	var imports []maven.Dependency
	var graph map[maven.Dependency][]maven.Dependency
	var err error
	if a.Options.Command == "" {
		imports, graph, err = a.Maven.DependencyTree(m.Dir, m.BuildTarget)
	} else {
		var output string
		output, _, err = exec.Shell(exec.Cmd{
			Command: a.Options.Command,
		})
		if err != nil {
			return m, err
		}
		imports, graph, err = maven.ParseDependencyTree(output)
	}
	if err != nil {
		return m, err
	}

	// Set direct dependencies.
	var i []pkg.Import
	for _, dep := range imports {
		i = append(i, pkg.Import{
			Resolved: pkg.ID{
				Type:     pkg.Maven,
				Name:     dep.Name,
				Revision: dep.Version,
			},
		})
	}

	// Set transitive dependencies.
	g := make(map[pkg.ID]pkg.Package)
	for parent, children := range graph {
		id := pkg.ID{
			Type:     pkg.Maven,
			Name:     parent.Name,
			Revision: parent.Version,
		}
		var imports []pkg.Import
		for _, child := range children {
			imports = append(imports, pkg.Import{
				Target: child.Version,
				Resolved: pkg.ID{
					Type:     pkg.Maven,
					Name:     child.Name,
					Revision: child.Version,
				},
			})
		}
		g[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	m.Imports = i
	m.Deps = g
	return m, nil
}
