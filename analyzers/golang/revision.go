package golang

import (
	"strings"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
)

// Revision resolves a revision, returning errutil.ErrNoRevisionForPackage
// when no revision is found (unless a revision is not required).
func (a *Analyzer) Revision(project Project, r resolver.Resolver, gopkg gocmd.Package) (pkg.Import, error) {
	if a.Options.AllowNestedVendor || a.Options.AllowDeepVendor {
		return a.RevisionNested(project, gopkg)
	}
	return a.RevisionFlattened(project, r, gopkg)
}

// RevisionFlattened resolves a revision in standard vendor folder layouts.
func (a *Analyzer) RevisionFlattened(project Project, r resolver.Resolver, gopkg gocmd.Package) (pkg.Import, error) {
	log.Logger.Debugf("Project: %#v", project)
	log.Logger.Debugf("Package: %#v", gopkg)

	name := Unvendor(gopkg.ImportPath)
	revision, err := r.Resolve(name)
	if err == errutil.ErrNoRevisionForPackage {
		log.Logger.Debugf("Could not find revision for package %#v", name)

		if a.UnresolvedOK(project, gopkg) {
			log.Logger.Debugf("Skipping package: %#v", gopkg)
			return UnresolvedImport(gopkg), nil
		}
	}
	if err != nil {
		return pkg.Import{}, err
	}
	return revision, nil
}

// RevisionNested resolves revisions, with support for nested vendor folders.
//
// Note that nested vendor folders should be deprecated by all build tools (they
// cause all sorts of weird failures), but some major projects (e.g.
// github.com/hashicorp/consul) still require support.
func (a *Analyzer) RevisionNested(project Project, gopkg gocmd.Package) (pkg.Import, error) {
	log.Logger.Debugf("%#v", gopkg)

	// Search upwards in parent directories.
	for dir := gopkg.Dir; strings.HasPrefix(dir, project.Dir) && a.Options.AllowDeepVendor; dir = VendorParent(dir) {
		log.Logger.Debugf("Trying dir: %#v", dir)

		// In each parent directory, try to get a resolver from a lockfile.
		tool, err := LockfileIn(dir)
		if err == ErrNoLockfileInDir {
			log.Logger.Debugf("No lockfile found.")
			continue
		}
		if err != nil {
			return pkg.Import{}, err
		}
		r, err := a.ResolverFromLockfile(tool, dir)
		if err != nil {
			return pkg.Import{}, err
		}

		// Try to resolve a revision using the lockfile.
		nestedProject, err := a.Project(gopkg.ImportPath)
		if err != nil {
			return pkg.Import{}, err
		}
		revision, err := a.RevisionFlattened(nestedProject, r, gopkg)
		if err == errutil.ErrNoRevisionForPackage {
			continue
		}
		if err != nil {
			return pkg.Import{}, err
		}
		return revision, nil
	}

	if a.UnresolvedOK(project, gopkg) {
		return UnresolvedImport(gopkg), nil
	}
	return pkg.Import{}, errutil.ErrNoRevisionForPackage
}

// UnresolvedOK returns true if failing to resolve the revision of a package
// is a non-fatal (rather than fatal) error.
//
// Packages require a resolved revision unless any of the following are true:
//
//   1. The package is part of the standard library.
//   2. The package is internal.
//   3. The package is within the project folder, but not under the vendor
//      folder.
//   4. A package at the project's folder would have an import path that is a
//      prefix of the package path.
//   5. Options.AllowUnresolved is true.
//   6. Options.AllowUnresolvedPrefix is set and is a prefix of the package's
//      import path.
//
// These exceptions are generally either "part of" (i.e. versioned as a unit
// with) the project, or otherwise don't have revisions.
func (a *Analyzer) UnresolvedOK(project Project, gopkg gocmd.Package) bool {
	withinDir :=
		strings.HasPrefix(gopkg.Dir, project.Dir) && strings.Index(gopkg.Dir, "/vendor/") == -1
	withinAllowedPrefix :=
		(a.Options.AllowUnresolvedPrefix != "" && strings.HasPrefix(Unvendor(gopkg.ImportPath), a.Options.AllowUnresolvedPrefix))

	return gopkg.IsStdLib ||
		gopkg.IsInternal ||
		withinDir ||
		strings.HasPrefix(Unvendor(gopkg.ImportPath), project.ImportPath) ||
		a.Options.AllowUnresolved ||
		withinAllowedPrefix
}
