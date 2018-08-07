package golang

import (
	"strings"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
)

// Revision resolves a revision, returning buildtools.ErrNoRevisionForPackage
// when no revision is found (unless a revision is not required).
func (a *Analyzer) Revision(project Project, r resolver.Resolver, gopkg gocmd.Package) (pkg.Import, error) {
	if a.Options.AllowNestedVendor ||
		a.Options.AllowDeepVendor ||
		a.Options.AllowExternalVendor ||
		a.Options.AllowExternalVendorPrefix != "" {
		return a.RevisionContextual(project, gopkg)
	}
	return a.RevisionDirect(project, r, gopkg)
}

// RevisionDirect resolves a revision by looking up its revision in the
// project's lockfile. This works for most conventional projects.
func (a *Analyzer) RevisionDirect(project Project, r resolver.Resolver, gopkg gocmd.Package) (pkg.Import, error) {
	log.Logger.Debugf("Project: %#v", project)
	log.Logger.Debugf("Package: %#v", gopkg)

	name := Unvendor(gopkg.ImportPath)
	revision, err := r.Resolve(name)
	if err == buildtools.ErrNoRevisionForPackage {
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

// RevisionContextual resolve a revision by looking up its revision in the
// lockfile of a vendoring parent. This supports complex use cases, such as
// multi-project vendoring (Docker), and nested vendor folders (Hashicorp):
//
//   1. Multi-project example: docker/docker-ce does not vendor all of its
//      dependencies within the project. Instead, it uses unvendored imports of
//      docker/docker, which vendors imports of its own set of dependencies.
//      This means that we need to do a lookup in docker/docker's lockfile to
//      resolve a transitive dependency of docker/docker-ce.
//   2. Nested vendor folder example: hashicorp/consul has nested vendor
//      folders. This means that resolving a transitive dependency may require
//      looking at the lockfile of the dependency that vendors it, or looking
//      further up at any of the ancestors in the vendoring chain.
//
// In theory, this is only used for setups that _build very carefully_, but are
// otherwise prone to exploding. In practice, this seems to be required a lot.
func (a *Analyzer) RevisionContextual(project Project, gopkg gocmd.Package) (pkg.Import, error) {
	log.Logger.Debugf("Project: %#v", project)
	log.Logger.Debugf("Package: %#v", gopkg)

	// Search upwards in parent directories.
	for dir := VendorParent(gopkg.Dir); dir != "." && a.ParentOK(project, gopkg, dir); dir = VendorParent(dir) {
		log.Logger.Debugf("Trying dir: %#v", dir)

		revision, err := a.RevisionContextualLookup(project, gopkg, dir)
		if err == ErrNoLockfileInDir {
			log.Logger.Debugf("No lockfile found.")
			if a.Options.AllowDeepVendor {
				continue
			} else {
				break
			}
		}
		if err != nil {
			return pkg.Import{}, err
		}

		return revision, nil
	}

	if a.UnresolvedOK(project, gopkg) {
		return UnresolvedImport(gopkg), nil
	}
	return pkg.Import{}, buildtools.ErrNoRevisionForPackage
}

// RevisionContextualLookup attempts to resolve a revision of a package using
// the lockfile in a specific directory.
func (a *Analyzer) RevisionContextualLookup(project Project, gopkg gocmd.Package, dir string) (pkg.Import, error) {
	log.Logger.Debugf("Trying dir: %#v", dir)

	// Try to get a resolver from a lockfile.
	tool, err := LockfileIn(dir)
	if err != nil {
		return pkg.Import{}, err
	}
	r, err := a.ResolverFromLockfile(tool, dir)
	if err != nil {
		return pkg.Import{}, err
	}

	// Try to resolve the revision.
	revision, err := a.RevisionDirect(project, r, gopkg)
	if err != nil {
		return pkg.Import{}, err
	}
	return revision, nil
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
//   6. Options.AllowUnresolvedPrefix is set one of its paths is a prefix of the
//      package's import path.
//
// These exceptions are generally either "part of" (i.e. versioned as a unit
// with) the project, or otherwise don't have revisions.
func (a *Analyzer) UnresolvedOK(project Project, gopkg gocmd.Package) bool {
	withinDir :=
		strings.HasPrefix(gopkg.Dir, project.Dir) && !strings.Contains(gopkg.Dir, "/vendor/")

	allowedPrefixes := strings.Split(a.Options.AllowUnresolvedPrefix, " ")
	hasAllowedPrefix := false
	for _, prefix := range allowedPrefixes {
		if strings.HasPrefix(Unvendor(gopkg.ImportPath), prefix) {
			hasAllowedPrefix = true
		}
	}
	withinAllowedPrefix := (a.Options.AllowUnresolvedPrefix != "" && hasAllowedPrefix)

	return gopkg.IsStdLib ||
		gopkg.IsInternal ||
		withinDir ||
		strings.HasPrefix(Unvendor(gopkg.ImportPath), project.ImportPath) ||
		a.Options.AllowUnresolved ||
		withinAllowedPrefix
}

// ParentOK returns true if looking up the lockfile of gopkg in dir is allowed.
//
// A package may be looked up in a directory if:
//
//   1. The directory is within the main project.
//   2. The directory is not within the main project, but we allow external
//      project vendoring.
//   3. The directory is not within the main project, but an allowed external
//      vendoring import path prefix is a prefix of the import path that a
//      package at the directory would have.
func (a *Analyzer) ParentOK(project Project, gopkg gocmd.Package, dir string) bool {
	log.Logger.Debugf("Project: %#v", project)
	log.Logger.Debugf("Package: %#v", gopkg)
	log.Logger.Debugf("Dir: %#v", dir)

	// If this returns an error, it should already have failed when creating the
	// project.
	dirImportPath, err := ImportPath(dir)
	if err != nil {
		panic(err)
	}
	allowedPrefixes := strings.Split(a.Options.AllowExternalVendorPrefix, " ")
	hasAllowedPrefix := false
	for _, prefix := range allowedPrefixes {
		if strings.HasPrefix(dirImportPath, prefix) {
			hasAllowedPrefix = true
		}
	}
	withinAllowedPrefix := (a.Options.AllowExternalVendorPrefix != "" && hasAllowedPrefix)

	return strings.HasPrefix(dir, project.Dir) || a.Options.AllowExternalVendor || withinAllowedPrefix
}
