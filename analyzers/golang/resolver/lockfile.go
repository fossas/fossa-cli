package resolver

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/buildtools/gdm"
	"github.com/fossas/fossa-cli/buildtools/glide"
	"github.com/fossas/fossa-cli/buildtools/godep"
	"github.com/fossas/fossa-cli/buildtools/gomodules"
	"github.com/fossas/fossa-cli/buildtools/govendor"
	"github.com/fossas/fossa-cli/buildtools/vndr"
)

// Errors from lockfile resolvers.
var (
	ErrResolverNotFound = errors.New("unrecognized Go resolver")
)

// Lockfile resolvers.
const (
	Dep       = Type("dep")
	Gdm       = Type("gdm")
	Glide     = Type("glide")
	GoModules = Type("gomodules")
	Godep     = Type("godep")
	Govendor  = Type("govendor")
	Vndr      = Type("vndr")
)

// FromLockfile constructs a resolver from a lockfile and its directory.
func FromLockfile(tool Type, dir string) (Resolver, error) {
	switch tool {
	case GoModules:
		return gomodules.New(dir)
	case Dep:
		return dep.New(filepath.Join(dir, "Gopkg.lock"), filepath.Join(dir, "Gopkg.toml"))
	case Gdm:
		return gdm.New(dir)
	case Glide:
		return glide.New(dir)
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
