package golang

// The project of a package is the nearest ancestor with a supported lockfile.
// Two packages in the same project can import each other without needing a
// revision in the lockfile, because these are ostensibly internal.
// Conceptually, anything in the same VCS project (and not in a vendor folder) should be an "internal" import
// because they'll all be versioned together.
// Looking upwards for a lockfile is a subset of this: it's never possible to
// have a lockfile "in between" directories that are not repositories (you'll
// never have github.com/hashicorp/Gopkg.lock), but your internal import might
// be higher up than the closest lockfile.
// alternatively: search upwards for .git or other VCS marker?
// A vendored package always in a different project than the one vendoring it.
func GetProjectFolder(pkg string) (string, error) {
	return "", nil
}

// i know the location of every package on disk.
// every package that goes up to the same VCS metadata folder _and_ is not in a
// vendor folder is in the same project.
//
// fall back to using lockfile location for project discovery _iff_ no VCS is
// found (e.g. new project without git init)
