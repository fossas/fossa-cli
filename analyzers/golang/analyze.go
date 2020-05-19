package golang

import (
	"path/filepath"

	"github.com/apex/log"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/buildtools/gdm"
	"github.com/fossas/fossa-cli/buildtools/glide"
	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/buildtools/godep"
	"github.com/fossas/fossa-cli/buildtools/gomodules"
	"github.com/fossas/fossa-cli/buildtools/govendor"
	"github.com/fossas/fossa-cli/buildtools/vndr"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze() (graph.Deps, error) {

	// 1. Check for a set strategy.
	switch a.Options.Strategy {
	case "gomodules":
		depGraph, err := gomodules.ModGraph("")
		if len(depGraph.Direct) > 0 && err == nil {
			return depGraph, nil
		}
		return gomodules.SumGraph("")
	case "dep":
		depGraph, err := dep.LockfileGraph("")
		if len(depGraph.Direct) > 0 && err == nil {
			return depGraph, nil
		}
		return dep.ManifestGraph("")
	case "list":
		return ListLockfileResolution(a)
	}

	// 2. Run `go list` and resolve using the lockfile.
	depGraph, err := ListLockfileResolution(a)
	if returnGraph(a.Module.Dir, err, len(depGraph.Direct)) {
		return depGraph, nil
	}

	// Get project directory to search for lockfiles.
	project, err := a.Project(a.Module.BuildTarget)

	// 3. Check for gomodules.
	if exists, err := files.Exists(filepath.Join(project.Dir, "go.mod")); exists && err == nil {
		depGraph, err := gomodules.ModGraph("")
		if returnGraph(filepath.Join(project.Dir, "go.mod"), err, len(depGraph.Direct)) {
			return depGraph, nil
		}
	}
	if exists, err := files.Exists(filepath.Join(project.Dir, "go.sum")); exists && err == nil {
		depGraph, err := gomodules.SumGraph("")
		if returnGraph(filepath.Join(project.Dir, "go.sum"), err, len(depGraph.Direct)) {
			return depGraph, nil
		}
	}

	// 4. Check for Dep.
	if exists, err := files.Exists(filepath.Join(project.Dir, "Gopkg.lock")); exists && err == nil {
		depGraph, err := dep.LockfileGraph("")
		if returnGraph(filepath.Join(project.Dir, "Gopkg.lock"), err, len(depGraph.Direct)) {
			return depGraph, nil
		}
	}
	if exists, err := files.Exists(filepath.Join(project.Dir, "Gopkg.toml")); exists && err == nil {
		depGraph, err := dep.ManifestGraph("")
		if returnGraph(filepath.Join(project.Dir, "Gopkg.toml"), err, len(depGraph.Direct)) {
			return depGraph, nil
		}
	}

	return graph.Deps{}, err
}

func returnGraph(file string, err error, dependencyCount int) bool {
	if err != nil {
		log.Warnf("Error reading dependencies from `%s`: %s", file, err)
		return false
	}

	if dependencyCount == 0 {
		log.Warnf("No dependencies were found in `%s`", file)
		return false
	}
	return true
}

func ListLockfileResolution(a *Analyzer) (graph.Deps, error) {
	m := a.Module
	log.Debugf("ListLockfileResolution module: %#v", m)
	// Get Go project.
	project, err := a.Project(m.BuildTarget)
	if err != nil {
		return graph.Deps{}, err
	}
	log.Debugf("Go project: %#v", project)

	// Read lockfiles to get revisions.
	var r resolver.Resolver
	switch a.Options.Strategy {
	case "manifest:gomodules":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = gomodules.New(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:dep":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = dep.New(a.Options.LockfilePath, a.Options.ManifestPath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:gdm":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = gdm.FromFile(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:glide":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = glide.FromFile(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:godep":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = godep.FromFile(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:govendor":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = govendor.FromFile(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}
	case "manifest:vndr":
		if a.Options.LockfilePath == "" {
			return graph.Deps{}, errors.New("manifest strategy specified without lockfile path")
		}
		r, err = vndr.FromFile(a.Options.LockfilePath)
		if err != nil {
			return graph.Deps{}, err
		}

	// Read revisions from an auto-detected tool manifest.
	default:
		r, err = a.ResolverFromLockfile(project.Tool, project.Manifest)
		if err != nil {
			return graph.Deps{}, err
		}
	}

	log.Debugf("Resolver: %#v", r)

	var allImports []pkg.Import
	importMap := make(map[pkg.Import]bool)
	transitiveDeps := make(map[pkg.ID]pkg.Package)

	for _, buildTag := range a.BuildTags {
		// Use `go list` to get imports and deps of module.
		flags := []string{"-tags", buildTag}
		main, err := a.Go.ListOne(m.BuildTarget, flags, a.Options.ModulesVendor)
		if err != nil {
			return graph.Deps{}, err
		}

		if len(main.Deps) == 0 {
			log.Warnf("No imports found for buid target %+v", m.BuildTarget)
			return graph.Deps{}, nil
		}

		log.Debugf("Go main package: %#v", main)
		deps, err := a.Go.List(main.Deps, flags, a.Options.ModulesVendor)
		if err != nil {
			return graph.Deps{}, err
		}

		// Construct map of import path to package.
		gopkgs := append(deps, main)
		gopkgMap := make(map[string]gocmd.Package)
		for _, p := range gopkgs {
			gopkgMap[p.ImportPath] = p
		}
		// cgo imports don't have revisions.
		gopkgMap["C"] = gocmd.Package{
			Name:     "C",
			IsStdLib: true, // This is so we don't try to lookup a revision. Maybe there should be a NoRevision bool field?
		}
		log.Debugf("gopkgMap: %#v", gopkgMap)

		// Construct transitive dependency graph.
		for _, gopkg := range deps {
			log.Debugf("Getting revision for: %#v", gopkg)

			// Resolve dependency.
			revision, err := a.Revision(project, r, gopkg)
			if err != nil {
				return graph.Deps{}, err
			}
			id := revision.Resolved

			// Check if the revision has already been scanned.
			if _, ok := transitiveDeps[id]; ok {
				continue
			}

			// Resolve dependency imports.
			var imports []pkg.Import
			for _, i := range gopkg.Imports {
				_, ok := gopkgMap[i]
				if !ok {
					log.Fatalf("Could not find Go package for %#v, your build may have errors. Try `go list -json <MODULE>`.", i)
				}
				log.Debugf("Resolving import of: %#v", gopkg)
				log.Debugf("Resolving dependency import: %#v", i)
				revision, err := a.Revision(project, r, gopkgMap[i])
				if err != nil {
					return graph.Deps{}, errors.Wrapf(err, "could not resolve %s", i)
				}
				imports = append(imports, revision)
			}

			transitiveDeps[id] = pkg.Package{
				ID:      id,
				Imports: imports,
			}
		}

		// Construct direct imports list.
		for _, i := range main.Imports {
			revision, err := a.Revision(project, r, gopkgMap[i])
			if err != nil {
				return graph.Deps{}, err
			}

			// Check if revision was added by a previous build tag.
			if _, exists := importMap[revision]; !exists {
				allImports = append(allImports, revision)
				importMap[revision] = true
			}
		}
	}

	m.Deps = transitiveDeps
	m.Imports = allImports

	return graph.Deps{
		Direct:     allImports,
		Transitive: transitiveDeps,
	}, nil
}
