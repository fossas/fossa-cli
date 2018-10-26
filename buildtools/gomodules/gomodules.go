package gomodules

import (
	"encoding/json"
	"strings"

	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/pkg"
)

// Resolver maps modules from import paths to their pkg.import representation.
type Resolver struct {
	pathMap map[string]pkg.Import
}

// module is used to unmarshal `go list -m -json all`.
type module struct {
	Path    string
	Version string
	Replace replace
}

type replace struct {
	Path    string
	Version string
}

// Resolve reduces a Go package to its module path and returns its revision
// contained within pathMap. buildtools.ErrNoRevisionForPackage is returned
// if the package cannot be found.
func (r Resolver) Resolve(importpath string) (pkg.Import, error) {
	splitPath := strings.Split(importpath, "/")
	for i := range splitPath {
		revision, ok := r.pathMap[importpath]
		if ok {
			return revision, nil
		}
		importpath = strings.TrimSuffix(importpath, "/"+splitPath[len(splitPath)-i-1])
	}
	return pkg.Import{}, buildtools.ErrNoRevisionForPackage
}

// New connects goModuleList and a parser to return a golang.Resolver.
func New(dir string) (Resolver, error) {
	moduleJSON, err := goModuleList(dir)
	if err != nil {
		return Resolver{}, errors.Wrap(err, "Could not run go list")
	}

	resolver, err := ParseModuleJSON(moduleJSON)
	if err != nil {
		return Resolver{}, errors.Wrap(err, "Could not parse json")
	}
	return resolver, nil
}

// ParseModuleJSON returns a golang.Resolver from the output of `go list -m -json all`.
// Replaced modules are handled in place and added to the pathMap.
func ParseModuleJSON(moduleJSON string) (Resolver, error) {
	resolver := Resolver{}
	var modList []module
	// The output for each module is valid JSON, but the output overall is not.
	err := json.Unmarshal([]byte("["+strings.Replace(moduleJSON, "}\n{", "},{", -1)+"]"), &modList)
	if err != nil {
		return resolver, errors.Wrap(err, "Could not unmarshal JSON into module list")
	}

	normalizedModules := make(map[string]pkg.Import)
	for _, mod := range modList {
		importpath := mod.Path

		// Handle replaced modules.
		emptyReplace := replace{}
		if mod.Replace != emptyReplace {
			mod = module{
				Version: mod.Replace.Version,
				Path:    mod.Replace.Path,
			}
		}

		version := extractRevision(mod.Version)
		normalizedModules[importpath] = pkg.Import{
			Target: version,
			Resolved: pkg.ID{
				Type:     pkg.Go,
				Name:     mod.Path,
				Revision: version,
			},
		}
	}

	resolver.pathMap = normalizedModules
	return resolver, nil
}

func goModuleList(path string) (string, error) {
	out, _, err := exec.Run(exec.Cmd{
		Name: "go",
		Argv: []string{"list", "-m", "-json", "all"},
		Dir:  path,
	})
	if err != nil {
		return "", errors.Wrapf(err, "Could not run `go list -m -json all` within the current directory: %s", path)
	}
	return out, nil
}

func extractRevision(version string) string {
	split := strings.Split(version, "-")
	if len(split) < 3 {
		return split[0]
	}
	return split[2]
}
