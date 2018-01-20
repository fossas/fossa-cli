package build

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/KyleBanks/depth"

	"github.com/BurntSushi/toml"
	yaml "gopkg.in/yaml.v2"
)

// GolangContext implements BuildContext for Golang projects
type GolangContext struct {
	GoCmd     string
	GoVersion string

	// dep
	UsingDep   bool
	DepCmd     string
	DepVersion string

	// glide
	UsingGlide   bool
	GlideCmd     string
	GlideVersion string

	// godep
	UsingGodep   bool
	GodepCmd     string
	GodepVersion string

	// govendor
	UsingGovendor   bool
	GovendorCmd     string
	GovendorVersion string

	// vndr
	UsingVndr   bool
	VndrCmd     string
	VndrVersion string

	// context, to avoid doing extra work
	tree *depth.Tree
	deps []Dependency
}

// Initialize gathers environment context.
func (ctx *GolangContext) Initialize(m *Module, opts map[string]interface{}) {
	// TODO: gather information about command versions.

	if _, err := os.Stat("Gopkg.lock"); err == nil {
		ctx.UsingDep = true
	}
	if _, err := os.Stat("Gopkg.toml"); err == nil {
		ctx.UsingDep = true
	}

	if _, err := os.Stat("glide.yaml"); err == nil {
		ctx.UsingGlide = true
	}
	if _, err := os.Stat("glide.lock"); err == nil {
		ctx.UsingGlide = true
	}

	if _, err := os.Stat("Godeps/Godeps.json"); err == nil {
		ctx.UsingGodep = true
	}

	if _, err := os.Stat("vendor/vendor.json"); err == nil {
		ctx.UsingGovendor = true
	}

	if _, err := os.Stat("vendor.conf"); err == nil {
		ctx.UsingVndr = true
	}
}

// Recursively flatten the dependency tree.
func getDeps(pkg *depth.Pkg) ([]Dependency, error) {
	var deps []Dependency
	for _, dep := range pkg.Deps {
		depDeps, err := getDeps(&dep)
		if err != nil {
			return nil, err
		}
		deps = append(deps, depDeps...)
	}

	// Ignore "internal" (i.e. standard library) packages.
	if pkg.Internal {
		if len(pkg.Deps) == 0 {
			return []Dependency{}, nil
		}
		return nil, errors.New("dependency of stdlib detected (this should never happen)")
	}
	// // Ignore "unresolved" packages (i.e. ones that are not in the filesystem).
	// // This is commented out because there are weird unresolved packages in import paths (e.g. URL redirectors)
	// if !pkg.Resolved {
	// 	return nil, errors.New("could not resolve package: " + pkg.Name)
	// }
	return append(deps, Gopkg{ImportPath: pkg.Name}), nil
}

// Verify checks whether dependencies are ready for scanning.
func (ctx *GolangContext) Verify(m *Module, opts map[string]interface{}) bool {
	// Prefer lockfiles where available.
	if ctx.UsingDep || ctx.UsingGlide || ctx.UsingGodep || ctx.UsingGovendor || ctx.UsingVndr {
		return true
	}

	// If no lockfiles are available, do import path tracing.
	var tree depth.Tree
	err := tree.Resolve(opts["entry-point"].(string))
	if err != nil {
		fmt.Printf("tree resolve error: %+v\n", err)
		fmt.Printf("tree: %+v\n", tree)
		return false
	}
	ctx.tree = &tree
	deps, err := getDeps(tree.Root)
	if err != nil {
		fmt.Printf("getDeps error: %+v\n", err)
		fmt.Printf("deps: %+v\n", deps)
		return false
	}
	ctx.deps = deps
	return true
}

type depLockfile struct {
	Projects []struct {
		Name     string
		Revision string
	}
}

type glideLockfile struct {
	Imports []struct {
		Name    string
		Version string
	}
}

type godepLockfile struct {
	Deps []struct {
		ImportPath string
		Rev        string
	}
}

type govendorLockfile struct {
	Package []struct {
		Path     string
		Revision string
	}
}

// Build scans for dependencies, building if necessary.
func (ctx *GolangContext) Build(m *Module, opts map[string]interface{}) error {
	var deps []Dependency

	// If possible, read lockfiles
	if ctx.UsingDep {
		if _, err := os.Stat("Gopkg.lock"); err != nil {
			return errors.New("project contains Gopkg.toml, but Gopkg.lock was not found")
		}
		lockfileContents, err := ioutil.ReadFile("Gopkg.lock")
		if err != nil {
			return errors.New("could not read Gopkg.lock")
		}
		var lockfile depLockfile
		if _, err := toml.Decode(string(lockfileContents), &lockfile); err != nil {
			return errors.New("could not parse Gopkg.lock")
		}
		for _, dependency := range lockfile.Projects {
			deps = append(deps, Gopkg{ImportPath: dependency.Name, Version: dependency.Revision})
		}
	} else if ctx.UsingGlide {
		if _, err := os.Stat("glide.lock"); err != nil {
			return errors.New("project contains glide.yaml, but glide.lock was not found")
		}
		lockfileContents, err := ioutil.ReadFile("glide.lock")
		if err != nil {
			return errors.New("could not read glide.lock")
		}
		var lockfile glideLockfile
		if err := yaml.Unmarshal(lockfileContents, &lockfile); err != nil {
			return errors.New("could not parse glide.lock")
		}
		for _, dependency := range lockfile.Imports {
			deps = append(deps, Gopkg{ImportPath: dependency.Name, Version: dependency.Version})
		}
	} else if ctx.UsingGodep {
		lockfileContents, err := ioutil.ReadFile("Godeps/Godeps.json")
		if err != nil {
			return errors.New("could not read Godeps/Godeps.json")
		}
		var lockfile godepLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return errors.New("could not parse Godeps/Godeps.json")
		}
		for _, dependency := range lockfile.Deps {
			deps = append(deps, Gopkg{ImportPath: dependency.ImportPath, Version: dependency.Rev})
		}
	} else if ctx.UsingGovendor {
		lockfileContents, err := ioutil.ReadFile("vendor/vendor.json")
		if err != nil {
			return errors.New("could not read vendor/vendor.json")
		}
		var lockfile govendorLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return errors.New("could not parse vendor/vendor.json")
		}
		for _, dependency := range lockfile.Package {
			deps = append(deps, Gopkg{ImportPath: dependency.Path, Version: dependency.Revision})
		}
	} else if ctx.UsingVndr {
		lockfileContents, err := ioutil.ReadFile("vendor.conf")
		if err != nil {
			return errors.New("could not read vendor.conf")
		}
		lines := strings.Split(string(lockfileContents), "\n")
		for _, line := range lines {
			trimmedLine := strings.TrimSpace(line)
			if len(trimmedLine) > 0 && trimmedLine[0] != '#' {
				sections := strings.Split(trimmedLine, " ")
				deps = append(deps, Gopkg{ImportPath: sections[0], Version: sections[1]})
			}
		}
	} else {
		// If no lockfiles are available, fall back to import path tracing.

		// Avoid recomputing context if `Verify` has already been called.
		tree := ctx.tree
		if ctx.tree == nil {
			err := tree.Resolve(opts["entry-point"].(string))
			if err != nil {
				return err
			}
		}

		vendoredDeps := ctx.deps
		if ctx.deps == nil {
			var err error
			// Don't use `:=`, otherwise you'll create a new binding that shadows the top-level binding
			vendoredDeps, err = getDeps(tree.Root)
			if err != nil {
				return err
			}
		}

		// Strip out `/vendor/` weirdness in import paths.
		for _, dep := range vendoredDeps {
			const vendorPrefix = "/vendor/"
			vendoredPathSections := strings.Split(dep.Package(), vendorPrefix)
			// Note that `Version` is unset for import path tracing.
			deps = append(deps, Gopkg{ImportPath: vendoredPathSections[len(vendoredPathSections)-1]})
		}
	}

	m.Build.Dependencies = deps
	return nil
}

// Gopkg implements Dependency for Golang projects.
type Gopkg struct {
	ImportPath string
	Version    string // This is actually the Git revision, but `.Revision()` is already taken.
}

// Fetcher returns "go".
func (g Gopkg) Fetcher() string {
	return "go"
}

// Package returns the package's import path.
func (g Gopkg) Package() string {
	return g.ImportPath
}

// Revision returns the package's resolved Git revision.
func (g Gopkg) Revision() string {
	return g.Version
}
