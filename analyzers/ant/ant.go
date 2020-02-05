package ant

import (
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/ant"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyzer implements build context for Ant builds
type Analyzer struct {
	Module  module.Module
	Options Options
}

type Options struct {
	LibDirectory string `mapstructure:"lib-directory"`
}

// New collects metadata on Java and Ant binaries.
func New(m module.Module) (*Analyzer, error) {
	log.Debugf("Initializing Ant builder...")

	// Decode options.
	var options Options
	err := mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	return &Analyzer{
		Module:  m,
		Options: options,
	}, nil
}

// Clean is currently not implemented.
func (a *Analyzer) Clean() error {
	return errors.New("Clean is not implemented for Ant")
}

// Build is currently not implemented.
func (a *Analyzer) Build() error {
	return errors.New("Build is not implemented for Ant")
}

// Analyze resolves a lib directory and parses the jars inside.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Debugf("Running Ant analysis: %#v in %s", a.Module, a.Module.Dir)

	libdir := "lib"
	if a.Options.LibDirectory != "" {
		libdir = a.Options.LibDirectory
	}

	log.Debugf("resolving ant libs in: %s", libdir)
	if ok, err := files.ExistsFolder(a.Module.Dir, libdir); !ok || err != nil {
		return graph.Deps{}, errors.New("unable to resolve library directory, try specifying it using the `modules.options.libdir` property in `.fossa.yml`")
	}

	deps, graphErr := ant.Graph(libdir)
	if graphErr != nil {
		return graph.Deps{}, graphErr
	}
	return deps, nil
}

// IsBuilt always returns true for Ant builds.
func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Discover returns any directory that contains a "build.xml" file and a "lib" directory.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	modules := []module.Module{}

	err := filepath.Walk(dir, func(filename string, info os.FileInfo, err error) error {
		if err != nil {
			log.WithError(err).WithField("filename", filename).Debug("failed to access path")
			return err
		}

		if !info.IsDir() && (info.Name() == "build.xml") {
			moduleDir := filepath.Dir(filename)
			libCheck, err := files.ExistsFolder(moduleDir, "lib")
			if err != nil {
				return errors.Wrap(err, "Error discovering ant modules")
			}

			if libCheck {
				moduleName := filepath.Base(moduleDir)
				log.WithFields(log.Fields{
					"path": filename,
					"name": moduleName,
				}).Debug("constructing Ant module")
				relPath, err := filepath.Rel(dir, filename)
				if err != nil {
					return errors.Wrap(err, "Error discovering ant modules")
				}

				modules = append(modules, module.Module{
					Name:        moduleName,
					Type:        pkg.Ant,
					BuildTarget: filepath.Dir(relPath),
					Dir:         filepath.Dir(relPath),
				})
			}
		}
		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "Could not find Ant package manifests: %s")
	}

	return modules, nil
}
