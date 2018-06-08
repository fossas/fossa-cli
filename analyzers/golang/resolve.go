package golang

import (
	"errors"

	"github.com/fossas/fossa-cli/pkg"
)

var (
	ErrResolverNotFound = errors.New("unrecognized Go resolver")
)

// A Resolver provides a single method for resolving the revision of a Go
// package.
type Resolver interface {
	Resolve(importpath string) pkg.ID
}

func NewResolver(resolver, dir string) (Resolver, error) {
	switch resolver {
	case "dep":
		return nil, errors.New("not yet implemented")
	case "gdm":
		return nil, errors.New("not yet implemented")
	case "glide":
		return nil, errors.New("not yet implemented")
	case "godep":
		return nil, errors.New("not yet implemented")
	case "govendor":
		return nil, errors.New("not yet implemented")
	case "vndr":
		return nil, errors.New("not yet implemented")
	default:
		return nil, ErrResolverNotFound
	}
}
