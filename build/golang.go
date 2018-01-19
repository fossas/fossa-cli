package build

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/BurntSushi/toml"
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
}

// Initialize gathers environment context.
func (ctx *GolangContext) Initialize(m *Module, opts map[string]interface{}) {
	// TODO: implement.
}

// Verify checks whether dependencies are ready for scanning.
func (ctx *GolangContext) Verify(m *Module, opts map[string]interface{}) bool {
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

	return ctx.UsingDep || ctx.UsingGlide || ctx.UsingGodep || ctx.UsingGovendor || ctx.UsingVndr
}

type depLockfile struct {
	Projects []struct {
		Name     string
		Packages []string
		Revision string
		Version  string
	}
}

// Build scans for dependencies, building if necessary.
func (ctx *GolangContext) Build(m *Module, opts map[string]interface{}) error {
	var deps []Dependency

	if ctx.UsingDep {
		if _, err := os.Stat("Gopkg.lock"); err != nil {
			return errors.New("project contains Gopkg.toml, but Gopkg.lock was not found")
		}
		depLockContents, err := ioutil.ReadFile("Gopkg.lock")
		if err != nil {
			return errors.New("could not read Gopkg.lock")
		}
		var depLock depLockfile
		if _, err := toml.Decode(string(depLockContents), &depLock); err != nil {
			return errors.New("could not parse Gopkg.lock")
		}
		for _, project := range depLock.Projects {
			deps = append(deps, Gopkg{ImportPath: project.Name, Version: project.Revision})
		}
	}

	fmt.Printf("%+v\n", deps)
	return errors.New("not implemented")
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
