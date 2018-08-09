// Package cocoapods implements Cocoapods analysis.
//
// A `BuildTarget` for Cocoapods is the path to the directory with the Podfile.
package cocoapods

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/mitchellh/mapstructure"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/buildtools/cocoapods"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	PodCmd     string
	PodVersion string

	Pod cocoapods.Cocoapods

	Module  module.Module
	Options Options
}

type Options struct{}

func New(m module.Module) (*Analyzer, error) {
	// Set Cocoapods context variables
	podCmd, podVersion, err := exec.Which("--version", os.Getenv("COCOAPODS_BINARY"), "pod")
	if err != nil {
		return nil, fmt.Errorf("could not find Cocoapods binary (try setting $COCOAPODS_BINARY): %s", err.Error())
	}

	// Parse and validate options.
	var options Options
	err = mapstructure.Decode(m.Options, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

	analyzer := Analyzer{
		PodCmd:     podCmd,
		PodVersion: podVersion,

		Pod: cocoapods.Cocoapods{
			Bin: podCmd,
		},

		Module:  m,
		Options: options,
	}

	log.WithField("analyzer", analyzer).Debug("constructed analyzer")
	return &analyzer, nil
}

// Discover constructs modules in all directories with a `Podfile`.
func Discover(dir string, options map[string]interface{}) ([]module.Module, error) {
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
		}

		if !info.IsDir() && info.Name() == "Podfile" {
			moduleName := filepath.Base(path)

			log.Logger.Debugf("Found Cocoapods package: %s (%s)", path, moduleName)
			relPath, _ := filepath.Rel(dir, path)
			modules = append(modules, module.Module{
				Name:        moduleName,
				Type:        pkg.Cocoapods,
				BuildTarget: filepath.Dir(relPath),
				Dir:         filepath.Dir(relPath),
			})
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("Could not find Cocoapods package manifests: %s", err.Error())
	}
	return modules, nil
}

// IsBuilt checks whether `Podfile.lock` exists
func (a *Analyzer) IsBuilt() (bool, error) {
	log.Logger.Debugf("Checking Cocoapods build: %#v", a.Module)

	isBuilt, err := files.Exists(a.Module.Dir, "Podfile.lock")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Cocoapods build: %#v", isBuilt)
	return isBuilt, nil
}

func (a *Analyzer) Clean() error {
	log.Logger.Warning("Clean is not implemented for Cocoapods")
	return nil
}

func (a *Analyzer) Build() error {
	return a.Pod.Install(a.Module.Dir)
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	lockfile, err := cocoapods.FromLockfile(a.Module.Dir, "Podfile.lock")
	if err != nil {
		return graph.Deps{}, err
	}

	// Construct a map of spec repositories.
	specRepos := make(map[string]string)
	for repo, pods := range lockfile.SpecRepos {
		for _, pod := range pods {
			specRepos[pod] = repo
		}
	}

	// Construct a map of all dependencies.
	nameToID := make(map[string]pkg.ID)
	for _, pod := range lockfile.Pods {
		// Get pod name: strip subspecs.
		name := PodName(pod.Name)
		id := pkg.ID{
			Type:     pkg.Cocoapods,
			Name:     name,
			Revision: pod.Version,
		}

		// Check if this pod is from an external source.
		if source, ok := lockfile.ExternalSources[pod.Name]; ok {
			if source.Git != "" {
				// Check if this pod is from a git repository.
				git, ok := lockfile.CheckoutOptions[pod.Name]
				if !ok {
					log.Logger.Warningf("Could not identify commit of git repository pod: %s", pod.Name)
				} else {
					// TODO: we should probably set this on Location instead, but this is
					// a quick hack because otherwise we'd have to special-case pod
					// handling to check Location.
					id.Type = pkg.Git
					id.Name = git.Git
					id.Revision = git.Commit
				}
			} else if source.Path != "" {
				// Check if this pod is vendored: we don't support this, but we can try
				// to look the name up as a Cocoapod.
			} else {
				log.Logger.Warningf("Could not identify externally sourced pod: %s", pod.Name)
			}
		}

		nameToID[name] = id
	}

	// Construct dependency graph edges.
	deps := make(map[pkg.ID]pkg.Package)
	for _, pod := range lockfile.Pods {
		id := nameToID[PodName(pod.Name)]

		var imports []pkg.Import
		for _, dep := range pod.Dependencies {
			imports = append(imports, pkg.Import{
				Target:   dep.String(),
				Resolved: nameToID[PodName(dep.Name)],
			})
		}

		deps[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	// Construct direct dependencies list.
	var imports []pkg.Import
	for _, dep := range lockfile.Dependencies {
		imports = append(imports, pkg.Import{
			Target:   dep.String(),
			Resolved: nameToID[PodName(dep.Name)],
		})
	}

	return graph.Deps{
		Direct:     imports,
		Transitive: deps,
	}, nil
}

// PodName strips subspecs. See https://guides.cocoapods.org/syntax/podspec.html#subspec
// for details.
func PodName(spec string) string {
	return strings.Split(spec, "/")[0]
}
