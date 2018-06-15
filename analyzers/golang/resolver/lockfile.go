package resolver

import (
	"errors"

	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/buildtools/godep"
	"github.com/fossas/fossa-cli/buildtools/govendor"
	"github.com/fossas/fossa-cli/buildtools/vndr"
)

// Errors from lockfile resolvers.
var (
	ErrResolverNotFound = errors.New("unrecognized Go resolver")
)

// Lockfile resolvers.
const (
	Dep      = Type("dep")
	Gdm      = Type("gdm")
	Glide    = Type("glide")
	Godep    = Type("godep")
	Govendor = Type("govendor")
	Vndr     = Type("vndr")
)

// FromLockfile constructs a resolver from a lockfile and its directory.
func FromLockfile(tool Type, dir string) (Resolver, error) {
	switch tool {
	case Dep:
		return dep.New(dir)
	case Gdm:
		return nil, errors.New("not yet implemented")
	case Glide:
		return nil, errors.New("not yet implemented")
	case Godep:
		return godep.New(dir)
	case Govendor:
		return govendor.New(dir)
	case Vndr:
		return vndr.New(dir)
	default:
		return nil, ErrResolverNotFound
	}
}
