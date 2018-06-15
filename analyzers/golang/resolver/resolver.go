// Package resolver provides Go resolvers.
package resolver

import "github.com/fossas/fossa-cli/pkg"

// Type is an enumeration of Resolver types. This helps clarify return
// signatures for functions that return many strings.
type Type string

// A Resolver provides a single method for resolving the revision of a Go
// package.
type Resolver interface {
	Resolve(importpath string) (pkg.Import, error)
}
