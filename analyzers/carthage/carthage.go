// A `BuildTarget` for Carthage is the path to the directory with the Cartfile.
package carthage

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/carthage"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	CarthageCmd     string
	CarthageVersion string

	Carthage carthage.Carthage

	Options Options
}

type Options struct{}

func New(opts map[string]interface{}) (*Analyzer, error) {
	// Set Carthage context variables
	cartCmd, cartVersion, err := exec.Which("version", os.Getenv("CARTHAGE_BINARY"), "carthage")
	if err != nil {
		return nil, fmt.Errorf("could not find Carthage binary (try setting $CARTHAGE_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(opts, &options)
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
	}

	log.Logger.Debugf("%#v", analyzer)
	return &analyzer, nil
}

// Discover constructs modules in all directories with a `Cartfile`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
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
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	log.Logger.Debugf("Checking Carthage build: %#v", m)
	hasResolvedCartfile, err := files.Exists(filepath.Join(m.Dir, "Cartfile.resolved"))

	if err != nil {
		log.Logger.Warningf("Error checking Carthage build: %#v", err.Error())
		return false, err
	}

	hasCarthageFolder, err := files.ExistsFolder(m.Dir, "Carthage")
	if err != nil {
		log.Logger.Warningf("Error checking Carthage build: %#v", err.Error())
		return false, err
	}

	isBuilt := hasCarthageFolder && hasResolvedCartfile

	log.Logger.Debugf("Done checking Carthage build: %#v", isBuilt)
	return isBuilt, nil
}

func (a *Analyzer) Clean(m module.Module) error {
	// we COULD do: files.Rm(m.Dir, "Carthage"), however sometimes this folder is checked in to source control
	log.Logger.Warning("Clean is not implemented for Carthage")
	return nil
}

func (a *Analyzer) Build(m module.Module) error {
	return a.Carthage.Install(m.Dir)
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("Running Carthage analysis: %#v", m)
	resolvedCartfile, err := carthage.FromResolvedCartfile("ROOT", m.Dir)
	if err != nil {
		return module.Module{}, err
	}
	cartfilePath := filepath.Join(m.Dir, "Cartfile.resolved")
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
	deps := make(map[pkg.ID]pkg.Package)
	recurseDeps(deps, resolvedCartfile)

	m.Imports = imports
	m.Deps = deps
	log.Logger.Debugf("Done running Carthage analysis: %#v", deps)
	return m, nil
}

func recurseDeps(pkgMap map[pkg.ID]pkg.Package, currentCartfile carthage.ResolvedCartfile) {
	log.Logger.Debugf("Searching Carthage deps for project %#v", currentCartfile.Name)
	for _, dep := range currentCartfile.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.Carthage,
			Name:     dep.Name,
			Revision: dep.Revision,
		}
		// Don't process duplicates.
		_, ok := pkgMap[id]
		if ok {
			continue
		}

		// Get direct imports.
		var imports make([]pkg.Import, 0)

		// Get Transitive Dep Info
		newCartfile, err := dep.CartfileFromRequirement(currentCartfile.Dir)

		if err != nil {
			log.Logger.Warningf("Error parsing Cartfile.resolved at %#v: %#v. Continuing...", currentCartfile.Dir, err.Error())
		} else {
			for _, i := range newCartfile.Dependencies {
				imports = append(imports, pkg.Import{
					Target: i.String(),
					Resolved: pkg.ID{
						Type:     pkg.Carthage,
						Name:     i.Name,
						Revision: i.Revision,
					},
				})
			}
		}
		// Update map.
		pkgMap[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		// Recurse in imports.
		recurseDeps(pkgMap, newCartfile)
	}
}
