// Package vcs implements functions for interacting with version control
// systems.
package vcs

import "github.com/fossas/fossa-cli/errutil"

type Type int

const (
	_ Type = iota
	Subversion
	Git
	Mercurial
	Bazaar
)

// GetRepository returns the location of the repository containing dirname, or
// errutil.ErrRepositoryNotFound on error.
func GetRepository(dirname string) (string, error) {
	return "", errutil.ErrNotImplemented
}
