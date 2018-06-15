package golang

import (
	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/pkg/errors"
)

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debug("%#v", m)

	// Get Go project.
	project, err := a.Project(m.BuildTarget)
	if err != nil {
		return m, err
	}
	log.Logger.Debugf("Go project: %#v", project)

	// Read lockfiles to get revisions.
	var r resolver.Resolver
	switch a.Options.Strategy {
	// Read revisions from a tool manifest at a specified location.
	// TODO: implement these strategies.
	case "manifest:dep":
		return m, errutil.ErrNotImplemented
	case "manifest:gdm":
		return m, errutil.ErrNotImplemented
	case "manifest:glide":
		return m, errutil.ErrNotImplemented
	case "manifest:godep":
		return m, errutil.ErrNotImplemented
	case "manifest:govendor":
		return m, errutil.ErrNotImplemented
	case "manifest:vndr":
		return m, errutil.ErrNotImplemented

	// Resolve revisions by traversing the local $GOPATH and calling the package's
	// VCS.
	case "gopath-vcs":
		return m, errutil.ErrNotImplemented

	// Read revisions from an auto-detected tool manifest.
	default:
		r, err = a.ResolverFromLockfile(project.Tool, project.Manifest)
		if err != nil {
			return m, err
		}
	}

	log.Logger.Debugf("Resolver: %#v", r)

	// Use `go list` to get imports and deps of module.
	main, err := a.Go.ListOne(m.BuildTarget)
	if err != nil {
		return m, err
	}
	log.Logger.Debugf("Go main package: %#v", main)
	deps, err := a.Go.List(main.Deps)
	if err != nil {
		return m, err
	}

	// Construct map of unvendored import path to package.
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

	// Construct transitive dependency graph.
	pkgs := make(map[pkg.ID]pkg.Package)
	for _, gopkg := range gopkgs {
		log.Logger.Debugf("Getting revision for: %#v", gopkg)

		// Resolve dependency.
		revision, err := a.Revision(project, r, gopkg)
		if err != nil {
			return m, err
		}
		id := revision.Resolved

		// Resolve dependency imports.
		var imports []pkg.Import
		for _, i := range gopkg.Imports {
			log.Logger.Debugf("Resolving import of: %#v", gopkg)
			log.Logger.Debugf("Resolving dependency import: %#v", i)
			revision, err := a.Revision(project, r, gopkgMap[i])
			if err != nil {
				return m, errors.Wrapf(err, "could not resolve %s", i)
			}
			imports = append(imports, revision)
		}

		pkgs[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	// Construct direct imports list.
	var imports []pkg.ID
	for _, i := range main.Imports {
		revision, err := a.Revision(project, r, gopkgMap[i])
		if err != nil {
			return m, err
		}

		imports = append(imports, revision.Resolved)
	}

	m.Deps = pkgs
	m.Imports = imports
	return m, nil
}
