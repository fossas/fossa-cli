package build

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"

	"github.com/KyleBanks/depth"
	"github.com/fossas/fossa-cli/log"

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

// Build a dependency list given an entry point.
func resolveManifest(m *Module) ([]Dependency, error) {
	// If no lockfiles are available, do import path tracing.
	var tree depth.Tree
	err := tree.Resolve(m.Manifest)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}
	deps, err := getDeps(tree.Root)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}
	return deps, nil
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
	// // Fail on "unresolved" packages (i.e. ones that are not in the filesystem) as they indicate an incomplete build.
	// // This is commented out because apparently some people have crazy builds.
	// if !pkg.Resolved {
	// 	return nil, errors.New("could not resolve package: " + pkg.Name)
	// }
	return append(deps, Gopkg{ImportPath: pkg.Name}), nil
}

// Verify checks whether dependencies are ready for scanning.
func (ctx *GolangContext) Verify(m *Module, opts map[string]interface{}) bool {
	deps, err := resolveManifest(m)
	if err != nil {
		log.Logger.Debugf("Could not resolve dependencies: %+v. Maybe you need to do a `go get`?\n", err)
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
	// Trace imports
	// Avoid recomputing context if `Verify` has already been called.
	var deps []Dependency
	var err error
	if ctx.deps == nil {
		// Don't use `:=`, otherwise you'll create a new binding that shadows the top-level binding
		deps, err = resolveManifest(m)

		// Attempt `go get` install on failure.
		if err != nil {
			if opts["install"].(bool) {
				log.Logger.Debug("Could not resolve some dependencies. Attempting to run build...")
				_, err = exec.Command("go", "get", m.Manifest).Output()
				if err != nil {
					return err
				}
				deps, err = resolveManifest(m)
				if err != nil {
					return err
				}
			} else {
				return err
			}
		}
	} else {
		deps = ctx.deps
	}

	// If possible, read lockfiles for versions
	lockfileVersions := make(map[string]string)

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
			lockfileVersions[dependency.Name] = dependency.Revision
		}
	}
	if ctx.UsingGlide {
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
			lockfileVersions[dependency.Name] = dependency.Version
		}
	}
	if ctx.UsingGodep {
		lockfileContents, err := ioutil.ReadFile("Godeps/Godeps.json")
		if err != nil {
			return errors.New("could not read Godeps/Godeps.json")
		}
		var lockfile godepLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return errors.New("could not parse Godeps/Godeps.json")
		}
		for _, dependency := range lockfile.Deps {
			lockfileVersions[dependency.ImportPath] = dependency.Rev
		}
	}
	if ctx.UsingGovendor {
		lockfileContents, err := ioutil.ReadFile("vendor/vendor.json")
		if err != nil {
			return errors.New("could not read vendor/vendor.json")
		}
		var lockfile govendorLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return errors.New("could not parse vendor/vendor.json")
		}
		for _, dependency := range lockfile.Package {
			lockfileVersions[dependency.Path] = dependency.Revision
		}
	}
	if ctx.UsingVndr {
		lockfileContents, err := ioutil.ReadFile("vendor.conf")
		if err != nil {
			return errors.New("could not read vendor.conf")
		}
		lines := strings.Split(string(lockfileContents), "\n")
		for _, line := range lines {
			trimmedLine := strings.TrimSpace(line)
			if len(trimmedLine) > 0 && trimmedLine[0] != '#' {
				sections := strings.Split(trimmedLine, " ")
				lockfileVersions[sections[0]] = sections[1]

			}
		}
	}

	var resolvedDeps []Dependency
	// Strip out `/vendor/` weirdness in import paths.
	for _, dep := range deps {
		const vendorPrefix = "/vendor/"
		vendoredPathSections := strings.Split(dep.Package(), vendorPrefix)
		importPath := vendoredPathSections[len(vendoredPathSections)-1]
		// Note that `Version` is unset for import path tracing.
		resolvedDeps = append(resolvedDeps, Gopkg{ImportPath: importPath, Version: lockfileVersions[importPath]})
	}

	m.Build.RawDependencies = Dedupe(resolvedDeps)
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
