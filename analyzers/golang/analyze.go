package golang

import (
	"fmt"

	"github.com/apex/log" // Each of these build tools provides a resolver.Resolver
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
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/pkg"
)

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	m := a.Module
	log.Debugf("%#v", m)

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

	// Resolve revisions by traversing the local $GOPATH and calling the package's
	// VCS.
	case "gopath-vcs":
		return graph.Deps{}, errors.ErrNotImplemented

	// Read revisions from an auto-detected tool manifest.
	default:
		r, err = a.ResolverFromLockfile(project.Tool, project.Manifest)
		if err != nil {
			return graph.Deps{}, err
		}
	}

	log.Debugf("Resolver: %#v", r)

	var tagImports []pkg.Import
	pkgs := make(map[pkg.ID]pkg.Package)

	tags := []string{"windows", "linux", "freebsd"}
	/* 	tags := []string{"windows", "linux", "freebsd", "android", "darwin", "dragonfly", "nacl", "netbsd", "openbsd", "plan9", "solaris", "386", "amd64", "amd64p32", "arm", "armbe", "arm64", "arm64be", "ppc64", "ppc64le", "mips", "mipsle", "mips64", "mips64le", "mips64p32", "mips64p32le", "ppc", "s390", "s390x", "sparc", "sparc64"} */

	for _, tag := range tags {
		fmt.Println(tag)
		// Use `go list` to get imports and deps of module.
		flags := []string{"-tags", tag}
		main, err := a.Go.ListOne(flags, m.BuildTarget)
		if err != nil {
			return graph.Deps{}, err
		}
		log.Debugf("Go main package: %#v", main)
		deps, err := a.Go.List(flags, main.Deps)
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
		/* 		pkgs := make(map[pkg.ID]pkg.Package)
		 */for _, gopkg := range deps {
			log.Debugf("Getting revision for: %#v", gopkg)

			// Resolve dependency.
			revision, err := a.Revision(project, r, gopkg)
			if err != nil {
				return graph.Deps{}, err
			}
			id := revision.Resolved

			// Check if the ID exists in pkgs. If not then go through with this
			// Should be much quicker because we're hitting a map in resolve and a map
			// here with pkgs.

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

			pkgs[id] = pkg.Package{
				ID:      id,
				Imports: imports,
			}
		}

		// Construct direct imports list.
		/* 		fmt.Println(main.Imports)
		 */fmt.Println(len(pkgs))
		for _, i := range main.Imports {
			revision, err := a.Revision(project, r, gopkgMap[i])
			if err != nil {
				return graph.Deps{}, err
			}

			if !alreadyImported(revision, tagImports) {
				tagImports = append(tagImports, revision)

			}
		}

		m.Deps = pkgs
		m.Imports = tagImports
	}
	return graph.Deps{
		Direct:     tagImports,
		Transitive: pkgs,
	}, nil
}

func alreadyImported(rev pkg.Import, imports []pkg.Import) bool {
	for _, revision := range imports {
		if revision == rev {
			return true
		}
	}
	return false
}
