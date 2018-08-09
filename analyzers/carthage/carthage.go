// A `BuildTarget` for Carthage is the path to the directory with the Cartfile.
package carthage

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/apex/log"
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/carthage"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	CarthageCmd     string
	CarthageVersion string

	Carthage carthage.Carthage

	Module  module.Module
	Options Options
}

type Options struct{}

func New(m module.Module) (*Analyzer, error) {
	// Set Carthage context variables
	cartCmd, cartVersion, err := exec.Which("version", os.Getenv("CARTHAGE_BINARY"), "carthage")
	if err != nil {
		return nil, fmt.Errorf("could not find Carthage binary (try setting $CARTHAGE_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	analyzer := Analyzer{
		CarthageCmd:     cartCmd,
		CarthageVersion: cartVersion,

		Carthage: carthage.Carthage{
			Bin: cartCmd,
		},

		Options: options,
		Module:  m,
	}

	log.WithField("analyzer", analyzer).Debug("constructed analyzer")
	return &analyzer, nil
}

// Discover constructs modules in all directories with a `Cartfile`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		moduleName := filepath.Base(path)

		if !info.IsDir() && (info.Name() == "Cartfile" || info.Name() == "Cartfile.private") {
			// If the same directory has both a Cartfile and Cartfile.private, we will just pick the Cartfile
			if info.Name() == "Cartfile.private" {
				cartfilePath := filepath.Join(filepath.Dir(path), "Cartfile")
				cartfileExists, err := files.Exists(cartfilePath)
				if err != nil {
					log.Logger.Debugf("Error searching for Cartfile: %s (%s)", path, err.Error())
				}

				if cartfileExists == true {
					return nil // This will get picked up before/later if it exists in the same directory
				}
			}

			log.Logger.Debugf("Found Carthage package: %s (%s)", path, moduleName)
			relPath, _ := filepath.Rel(dir, path)
			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Carthage,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Carthage package manifests: %s", err.Error())
	}

	return modules, nil
}

// IsBuilt checks whether file `Cartfile.resolved` exists, and also if a `Carthage` folder exists
func (a *Analyzer) IsBuilt() (bool, error) {
	log.Logger.Debugf("Checking Carthage build: %#v", a.Module)
	hasResolvedCartfile, err := files.Exists(filepath.Join(a.Module.Dir, "Cartfile.resolved"))

	if err != nil {
		log.Logger.Warningf("Error checking Carthage build: %#v", err.Error())
		return false, err
	}

	hasCarthageFolder, err := files.ExistsFolder(a.Module.Dir, "Carthage")
	if err != nil {
		log.Logger.Warningf("Error checking Carthage build: %#v", err.Error())
		return false, err
	}

	isBuilt := hasCarthageFolder && hasResolvedCartfile

	log.Logger.Debugf("Done checking Carthage build: %#v", isBuilt)
	return isBuilt, nil
}

func (a *Analyzer) Clean() error {
	return files.Rm(a.Module.Dir, "Carthage")
}

func (a *Analyzer) Build() error {
	return a.Carthage.Install(a.Module.Dir)
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.Logger.Debugf("Running Carthage analysis: %#v", a.Module)
	resolvedCartfile, err := carthage.FromResolvedCartfile("ROOT", a.Module.Dir)
	if err != nil {
		return graph.Deps{}, err
	}
	cartfilePath := filepath.Join(a.Module.Dir, "Cartfile.resolved")
	// Set direct dependencies.
	var imports []pkg.Import
	for _, dep := range resolvedCartfile.Dependencies {
		imports = append(imports, pkg.Import{
			Target: dep.String(),
			Resolved: pkg.ID{
				Type:     pkg.Carthage,
				Name:     dep.Name,
				Revision: dep.Revision,
				Location: cartfilePath,
			},
		})
	}

	// Set transitive dependencies.
	transitiveDeps := make(map[pkg.ID]pkg.Package)
	carthage.RecurseDeps(transitiveDeps, resolvedCartfile)

	log.Logger.Debugf("Done running Carthage analysis: %#v", transitiveDeps)

	return graph.Deps{
		Direct:     imports,
		Transitive: transitiveDeps,
	}, nil

}
