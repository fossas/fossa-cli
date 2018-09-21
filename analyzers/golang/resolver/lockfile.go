package resolver

import (
	"errors"
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools/dep"
	"github.com/fossas/fossa-cli/buildtools/gdm"
	"github.com/fossas/fossa-cli/buildtools/glide"
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
		return dep.New(filepath.Join(dir, "Gopkg.lock"), filepath.Join(dir, "Gopkg.toml"))
	case Gdm:
		return gdm.New(filepath.Join(dir, "Godeps"))
	case Glide:
		return glide.New(filepath.Join(dir, "glide.lock"), filepath.Join(dir, "glide.yaml"))
	case Godep:
		return godep.New(filepath.Join(dir, "Godeps", "Godeps.json"))
	case Govendor:
		return govendor.New(filepath.Join(dir, "vendor", "vendor.json"))
	case Vndr:
		return vndr.New(filepath.Join(dir, "vendor.conf"))
	default:
		return nil, ErrResolverNotFound
	}
}
