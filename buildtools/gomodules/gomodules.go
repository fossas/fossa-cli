package gomodules

import (
	"encoding/json"
	"os"
	"path"
	"strings"

	"github.com/fossas/fossa-cli/buildtools"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/pkg"
)

// Resolver maps modules from import paths to their pkg.Import representation.
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

// Resolve calculates a package's module path from its import path,
// and returns the resolved pkg.Import for the module path.
func (r Resolver) Resolve(importpath string) (pkg.Import, error) {
	for p := importpath; p != "." && p != "/"; p = path.Dir(p) {
		revision, ok := r.pathMap[p]
		if ok {
			return revision, nil
		}
	}
	return pkg.Import{}, buildtools.ErrNoRevisionForPackage
}

// New connects goModuleList and a parser to return a golang.Resolver.
func New(dir string) (Resolver, error) {
	moduleJSON, err := goModuleList(dir)
	if err != nil {
		return Resolver{}, errors.Wrap(err, "Could not run go list")
	}

	resolver, err := parseModuleJSON(moduleJSON)
	if err != nil {
		return Resolver{}, errors.Wrap(err, "Could not parse json")
	}
	return resolver, nil
}

// Mock creates a golang.Resolver using any string input.
func Mock(modules string) (Resolver, error) {
	return parseModuleJSON(modules)
}

// parseModuleJSON returns a golang.Resolver from the output of `go list -m -json all`.
// Replaced modules are handled in place and added to the pathMap.
func parseModuleJSON(moduleJSON string) (Resolver, error) {
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
	cmd, _, err := exec.Which("version", os.Getenv("FOSSA_GO_CMD"), "go")
	if err != nil {
		return "", err
	}
	stdout, stderr, err := exec.Run(exec.Cmd{
		Name: cmd,
		Argv: []string{"list", "-m", "-json", "all"},
		Dir:  path,
	})
	if err != nil {
		return "", errors.Errorf("Could not run `go list -m -json all` within path %s: %s (%s)", path, strings.TrimSpace(stderr), err)
	}

	return stdout, nil
}

// extractRevision returns the correct revision from a gomodules version which has
// the form of "version-date-gitSHA" or "version". If gitSHA is present we want to
// return it as this means version is v0.0.0.
func extractRevision(version string) string {
	split := strings.Split(version, "-")
	if len(split) < 3 {
		return strings.TrimSuffix(version, "+incompatible")
	}
	return split[2]
}
