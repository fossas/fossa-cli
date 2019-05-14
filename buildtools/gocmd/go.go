// Package gocmd provides functions for working with the Go tool. This package
// is not named "go" because that name collides with a reserved keyword.
package gocmd

import (
	"encoding/json"
	"strings"

	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
)

// Package represents a single Go package.
type Package struct {
	Name       string      // Vendored import name (what would go into an `import` statement).
	ImportPath string      // Fully qualified import name (including `vendor` folders).
	Dir        string      // Absolute location on filesystem.
	IsInternal bool        // Whether the package is an internal package.
	IsStdLib   bool        // Whether the package is a part of the standard library.
	Imports    []string    // Direct dependencies.
	Deps       []string    // Transitive dependencies.
	Error      interface{} // A package loading error, if applicable.
}

// Name computes the vendored import path of a package given its fully qualified
// import name.
func Name(importPath string) string {
	sections := strings.Split(importPath, "vendor")
	return sections[len(sections)-1]
}

// Go contains configuration information for the Go tool.
type Go struct {
	Cmd string
	Dir string
}

// GoListOutput is a subset of the output of `go list`. See `go help list` for
// details.
type GoListOutput struct {
	Name       string
	ImportPath string
	Dir        string
	Standard   bool
	Imports    []string
	Deps       []string
	Error      *GoListPackageError
	DepsErrors []*GoListPackageError
}

// GoListPackageError is the PackageError struct defined in `go help list`.
type GoListPackageError struct {
	ImportStack []string
	Pos         string
	Err         string
}

// ListOne runs List for a single package.
func (g *Go) ListOne(pkg string, flags []string) (Package, error) {
	pkgs, err := g.List([]string{pkg}, flags)
	if err != nil {
		return Package{}, err
	}
	if len(pkgs) != 1 {
		return Package{}, errors.New("go build target specifies multiple packages")
	}
	return pkgs[0], nil
}

// List runs `go list` to return information about packages.
func (g *Go) List(pkgs, flags []string) ([]Package, error) {
	// Run `go list -json $PKG` and unmarshal output.
	var output []GoListOutput
	argv := append(flags, pkgs...)
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: g.Cmd,
		Argv: append([]string{"list", "-json"}, argv...),
		Dir:  g.Dir,
	})
	if err != nil && stdout == "" {
		if strings.Contains(stderr, "build constraints exclude all Go files") {
			// TODO: add better documentation around this error, and rename it to be
			// more useful.
			return nil, errors.New("bad OS/architecture target")
		}
		return nil, errors.Errorf("could not run go list: %s (%s)", strings.TrimSpace(stderr), err)
	}
	// The output for each package is valid JSON, but the output overall is not
	// valid JSON until we massage it a bit.
	err = json.Unmarshal([]byte("["+strings.Replace(stdout, "}\n{", "},{", -1)+"]"), &output)
	if err != nil {
		return nil, errors.Wrap(err, "could not unmarshal go list output")
	}

	// Parse output into Packages.
	var ret []Package
	for _, pkg := range output {
		p := Package{
			Name:       pkg.Name,
			ImportPath: pkg.ImportPath,
			Dir:        pkg.Dir,
			IsInternal: strings.Contains(pkg.ImportPath, "internal"),
			IsStdLib:   pkg.Standard,
			Imports:    pkg.Imports,
			Deps:       pkg.Deps,
		}
		if pkg.DepsErrors != nil {
			p.Error = pkg.DepsErrors
		}
		if pkg.Error != nil {
			p.Error = pkg.Error
		}
		ret = append(ret, p)
	}
	return ret, nil
}

// Build runs `go build` for packages.
func (g *Go) Build(pkgs []string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: g.Cmd,
		Argv: append([]string{"build"}, pkgs...),
	})
	return err
}

// Clean runs `go clean` for packages.
func (g *Go) Clean(pkgs []string) error {
	_, _, err := exec.Run(exec.Cmd{
		Name: g.Cmd,
		Argv: append([]string{"clean"}, pkgs...),
	})
	return err
}
