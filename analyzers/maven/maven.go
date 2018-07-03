package maven

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/files"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/maven"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	JavaCmd     string
	JavaVersion string

	MavenCmd     string
	MavenVersion string

	Maven   maven.Maven
	Options Options
}

type Options struct {
	Flags string
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debug("Initializing Maven analyzer...")

	// Set Java context variables
	javaCmd, javaVersion, err := exec.Which("-version", os.Getenv("JAVA_BINARY"), "java")
	if err != nil {
		log.Logger.Warningf("Could not find Java binary (try setting $JAVA_BINARY): %s", err.Error())
	}

	// Set Maven context variables
	mavenCmd, mavenVersion, err := exec.Which("--version", os.Getenv("MAVEN_BINARY"), "mvn")
	if err != nil {
		return nil, fmt.Errorf("could not find Maven binary (try setting $MAVEN_BINARY): %s", err.Error())
	}

	var options Options
	err = mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		JavaCmd:     javaCmd,
		JavaVersion: javaVersion,

		MavenCmd:     mavenCmd,
		MavenVersion: mavenVersion,

		Maven: maven.Maven{
			Cmd: mavenCmd,
		},
		Options: options,
	}
	log.Logger.Debugf("Done initializing Maven analyzer: %#v", analyzer)
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
			if err != nil {
				log.Logger.Debugf("Modules err: %#v %#v", err.Error(), err)
				return err
			}

			for _, m := range submodules {
				modules = append(modules, module.Module{
					Name:        m,
					Type:        pkg.Maven,
					BuildTarget: m,
					Dir:         dir,
				})
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
	imports, graph, err := a.Maven.DependencyTree(m.Dir, m.BuildTarget)
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
