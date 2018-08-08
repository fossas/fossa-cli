// Package bower implements analyzers for the Bower package manager.
//
// A `BuildTarget` for bower is the path to the `bower.json` of a project.
package bower

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/bower"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/apex/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	BowerCmd     string
	BowerVersion string

	Bower bower.Bower

	Module  module.Module
	Options Options
}

// TODO: strategies:
// - bower list
// - read components
// - read manifest

type Options struct {
	Strategy      string `mapstructure:"strategy"`
	ComponentsDir string `mapstructure:"components"`
}

func New(m module.Module) (*Analyzer, error) {
	log.Logger.Debug("%#v", m.Options)
	// Set Bower context variables
	bowerCmd, bowerVersion, err := exec.Which("-v", os.Getenv("BOWER_BINARY"), "bower")
	if err != nil {
		return nil, errors.Wrap(err, "could not find Bower binary (try setting $BOWER_BINARY)")
	}

	// Decode options
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}

	b, err := bower.New(bowerCmd, m.Dir)
	if err != nil {
		return nil, errors.Wrap(err, "could not set up Bower")
	}

	analyzer := Analyzer{
		BowerCmd:     bowerCmd,
		BowerVersion: bowerVersion,
		Bower:        *b,

		Module:  m,
		Options: options,
	}

	log.Logger.Debugf("analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover finds any `bower.json`s not in `node_modules` or `bower_components`
// folders.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var moduleConfigs []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s", path, err.Error())
			return err
		}
		// Skip **/node_modules and **/bower_components directories
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Logger.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "bower.json" {
			dir := filepath.Dir(path)
			name := filepath.Base(dir)

			// Parse from bower.json and set name if successful
			manifest, err := bower.ReadManifest(path)
			if err == nil {
				name = manifest.Name
			}

			log.Logger.Debugf("Found Bower package: %s (%s)", path, name)
			moduleConfigs = append(moduleConfigs, module.Module{
				Name:        name,
				Type:        pkg.Bower,
				BuildTarget: path,
				Dir:         dir,
			})
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("could not find bower package manifests: %s", err.Error())
	}

	return moduleConfigs, nil
}

func (a *Analyzer) Clean() error {
	return a.Bower.Clean()
}

// Build runs `bower install --production`
func (a *Analyzer) Build() error {
	return a.Bower.Install(true)
}

// IsBuilt checks for the existence of a components folder.
func (a *Analyzer) IsBuilt() (bool, error) {
	config, err := bower.ReadConfig(a.Module.Dir)
	if err != nil {
		return false, err
	}
	// TODO: Check if the installed modules are consistent with what's in the
	// actual manifest.
	bowerComponentsDir := config.Directory
	if !filepath.IsAbs(bowerComponentsDir) {
		bowerComponentsDir = filepath.Join(a.Module.Dir, config.Directory)
	}
	isBuilt, err := files.ExistsFolder(bowerComponentsDir)
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Bower build: %#v", isBuilt)
	return isBuilt, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	p, err := a.Bower.List()
	if err != nil {
		return graph.Deps{}, err
	}

	var imports []pkg.Import
	for _, dep := range p.Dependencies {
		imports = append(imports, pkg.Import{
			Target: dep.PkgMeta.TargetName + "@" + dep.PkgMeta.TargetVersion,
			Resolved: pkg.ID{
				Type:     pkg.Bower,
				Name:     dep.PkgMeta.Name,
				Revision: dep.PkgMeta.Version,
				Location: dep.Endpoint.Source,
			},
		})
	}

	deps := make(map[pkg.ID]pkg.Package)
	recurseDeps(deps, p)

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

func recurseDeps(pkgMap map[pkg.ID]pkg.Package, p bower.Package) {
	for name, dep := range p.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.Bower,
			Name:     name,
			Revision: dep.PkgMeta.Version,
			Location: dep.Endpoint.Source,
		}
		// Don't process duplicates.
		_, ok := pkgMap[id]
		if ok {
			continue
		}
		// Get direct imports.
		var imports []pkg.Import
		for name, i := range p.Dependencies {
			imports = append(imports, pkg.Import{
				Target: i.PkgMeta.TargetName + "@" + i.PkgMeta.TargetVersion,
				Resolved: pkg.ID{
					Type:     pkg.Bower,
					Name:     name,
					Revision: i.PkgMeta.Version,
					Location: i.Endpoint.Source,
				},
			})
		}
		// Update map.
		pkgMap[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		// Recurse in imports.
		recurseDeps(pkgMap, dep)
	}
}
