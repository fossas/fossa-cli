package build

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/KyleBanks/depth"
	logging "github.com/op/go-logging"
	yaml "gopkg.in/yaml.v2"

	"github.com/fossas/fossa-cli/module"
)

var goLogger = logging.MustGetLogger("golang")

// GoPkg implements Dependency for Golang projects.
type GoPkg struct {
	ImportPath string
	Version    string // This is actually the Git revision, but `.Revision()` is already taken.
}

// Fetcher returns "go".
func (g GoPkg) Fetcher() string {
	return "go"
}

// Package returns the package's import path.
func (g GoPkg) Package() string {
	return g.ImportPath
}

// Revision returns the package's resolved Git revision.
func (g GoPkg) Revision() string {
	return g.Version
}

// GoBuilder implements Builder for Golang projects
type GoBuilder struct {
	GoCmd     string
	GoVersion string

	// dep
	DepCmd     string
	DepVersion string

	// glide
	GlideCmd     string
	GlideVersion string

	// godep
	GodepCmd     string
	GodepVersion string

	// govendor
	GovendorCmd     string
	GovendorVersion string

	// vndr
	VndrCmd     string
	VndrVersion string
}

// Initialize gathers environment context.
func (builder *GoBuilder) Initialize() error {
	// TODO: gather information about command names and versions.
	return nil
}

// Build scans for dependencies, building if necessary.
func (builder *GoBuilder) Build(m module.Module, force bool) error {
	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return err
	}
	if !ok {
		return errors.New("did not find buildable project (maybe your go build tool is not supported?)")
	}
	goLogger.Debugf("Found project folder: %s\n", projectFolder)

	// TODO: check and use builder.*Cmd
	if _, err := os.Stat(filepath.Join(projectFolder, "Gopkg.toml")); err == nil {
		if force {
			cmd := exec.Command("rm", "-rf", "vendor", "Gopkg.lock")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `dep` cache: %s", err.Error())
			}
		}
		cmd := exec.Command("dep", "ensure")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `dep ensure`: %s", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "glide.yaml")); err == nil {
		if force {
			cmd := exec.Command("rm", "-rf", "vendor", "glide.lock")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `glide` cache: %s", err.Error())
			}
		}
		cmd := exec.Command("glide", "install")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `glide install`: %s", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "Godeps/Godeps.json")); err == nil {
		if force {
			cmd := exec.Command("rm", "-rf", "vendor")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `godep` cache: %s", err.Error())
			}
		}
		cmd := exec.Command("godep", "restore")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `godep restore`: %s", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "vendor/vendor.json")); err == nil {
		if force {
			backupCmd := exec.Command("mv", "vendor/vendor.json", "vendor.json.bak")
			backupCmd.Dir = projectFolder
			_, err := backupCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %s", err.Error())
			}
			cleanCmd := exec.Command("rm", "-rf", "vendor")
			cleanCmd.Dir = projectFolder
			_, err = cleanCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %s", err.Error())
			}
			makeFolderCmd := exec.Command("mkdir", "-p", "vendor")
			makeFolderCmd.Dir = projectFolder
			_, err = makeFolderCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %s", err.Error())
			}
			restoreCmd := exec.Command("mv", "vendor.json.bak", "vendor/vendor.json")
			restoreCmd.Dir = projectFolder
			_, err = restoreCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %s", err.Error())
			}
		}
		cmd := exec.Command("govendor", "sync", "+local")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `govendor sync`: %s", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "vendor.conf")); err == nil {
		if force {
			cmd := exec.Command("rm", "-rf", "vendor")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `vndr` cache: %s", err.Error())
			}
		}
		cmd := exec.Command("vndr")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `vndr`: %s", err.Error())
		}
	}

	// TODO: use builder.GoCmd
	cmd := exec.Command("go", "get", m.Target)
	cmd.Dir = projectFolder
	_, err = cmd.Output()
	if err != nil {
		return fmt.Errorf("could not build gopkg %s: %s", m.Target, err.Error())
	}

	return nil
}

// Build a dependency list given an entry point.
func traceImports(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	// If no lockfiles are available, do import path tracing.
	var tree depth.Tree
	err := tree.Resolve(m.Target)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}
	deps, err := flattenDepTree(*tree.Root, allowUnresolved)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}
	return deps, nil
}

// Recursively flatten the dependency tree.
func flattenDepTree(pkg depth.Pkg, allowUnresolved bool) ([]module.Dependency, error) {
	var deps []module.Dependency
	for _, dep := range pkg.Deps {
		flattened, err := flattenDepTree(dep, allowUnresolved)
		if err != nil {
			return nil, err
		}
		deps = append(deps, flattened...)
	}

	// Ignore "internal" (i.e. standard library) packages.
	if pkg.Internal {
		if len(pkg.Deps) == 0 {
			return []module.Dependency{}, nil
		}
		return nil, errors.New("dependency of stdlib detected (this should never happen)")
	}

	if allowUnresolved || pkg.Resolved {
		return append(deps, GoPkg{ImportPath: pkg.Name}), nil
	}
	return nil, errors.New("could not resolve package: " + pkg.Name)
}

type depLockfile struct {
	Projects []struct {
		Name     string
		Revision string
	}
}

func hasDepManifest(path string) (bool, error) {
	return hasFile(path, "Gopkg.toml")
}

type glideLockfile struct {
	Imports []struct {
		Name    string
		Version string
	}
}

func hasGlideManifest(path string) (bool, error) {
	return hasFile(path, "glide.yaml")
}

type godepLockfile struct {
	Deps []struct {
		ImportPath string
		Rev        string
	}
}

func hasGodepManifest(path string) (bool, error) {
	return hasFile(path, "Godeps", "Godeps.json")
}

type govendorLockfile struct {
	Package []struct {
		Path     string
		Revision string
	}
}

func hasGovendorManifest(path string) (bool, error) {
	return hasFile(path, "vendor", "vendor.json")
}

func hasVndrManifest(path string) (bool, error) {
	return hasFile(path, "vendor.conf")
}

func hasFile(elem ...string) (bool, error) {
	_, err := os.Stat(filepath.Join(elem...))
	if os.IsNotExist(err) {
		return false, nil
	}
	return !os.IsNotExist(err), err
}

func orPredicates(predicates ...stopPredicate) stopPredicate {
	return func(path string) (bool, error) {
		for _, predicate := range predicates {
			ok, err := predicate(path)
			if err != nil {
				return false, err
			}
			if ok {
				return ok, nil
			}
		}
		return false, nil
	}
}

type stopPredicate func(path string) (bool, error)

func findAncestor(stopWhen stopPredicate, path string) (string, bool, error) {
	absPath, err := filepath.Abs(path)
	if absPath == string(filepath.Separator) {
		return "", false, nil
	}
	if err != nil {
		return "", false, err
	}
	stop, err := stopWhen(absPath)
	if err != nil {
		return "", false, err
	}
	if stop {
		return absPath, true, nil
	}
	return findAncestor(stopWhen, filepath.Dir(path))
}

func findGoProjectFolder(fromPath string) (string, bool, error) {
	return findAncestor(
		orPredicates(
			hasDepManifest,
			hasGlideManifest,
			hasGodepManifest,
			hasGovendorManifest,
			hasVndrManifest,
		), fromPath)
}

func (builder *GoBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	// Trace imports
	deps, err := traceImports(m, allowUnresolved)
	if err != nil {
		return nil, fmt.Errorf("could not trace imports during analysis: %s", err.Error())
	}
	goLogger.Debugf("Analyze tracing succeeded\n")

	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return nil, err
	}
	if !ok {
		goLogger.Debugf("Could not find project folder\n")
		return deps, err
	}
	goLogger.Debugf("Found project folder: %s\n", projectFolder)

	// If possible, read lockfiles for versions
	lockfileVersions := make(map[string]string)

	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `dep` manifest\n")
		if ok, err := hasFile(projectFolder, "Gopkg.lock"); err != nil && ok {
			return nil, errors.New("project contains Gopkg.toml, but Gopkg.lock was not found")
		}
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "Gopkg.lock"))
		if err != nil {
			return nil, errors.New("could not read Gopkg.lock")
		}
		var lockfile depLockfile
		if _, err := toml.Decode(string(lockfileContents), &lockfile); err != nil {
			return nil, errors.New("could not parse Gopkg.lock")
		}
		for _, dependency := range lockfile.Projects {
			lockfileVersions[dependency.Name] = dependency.Revision
		}
	}
	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `glide` manifest\n")
		if ok, err := hasFile(projectFolder, "glide.lock"); err != nil && ok {
			return nil, errors.New("project contains glide.yaml, but glide.lock was not found")
		}
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "glide.lock"))
		if err != nil {
			return nil, errors.New("could not read glide.lock")
		}
		var lockfile glideLockfile
		if err := yaml.Unmarshal(lockfileContents, &lockfile); err != nil {
			return nil, errors.New("could not parse glide.lock")
		}
		for _, dependency := range lockfile.Imports {
			lockfileVersions[dependency.Name] = dependency.Version
		}
	}
	if ok, err := hasGodepManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `Godeps` manifest\n")
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "Godeps", "Godeps.json"))
		if err != nil {
			return nil, errors.New("could not read Godeps/Godeps.json")
		}
		var lockfile godepLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return nil, errors.New("could not parse Godeps/Godeps.json")
		}
		for _, dependency := range lockfile.Deps {
			lockfileVersions[dependency.ImportPath] = dependency.Rev
		}
	}
	if ok, err := hasGovendorManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `govendor` manifest\n")
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "vendor", "vendor.json"))
		if err != nil {
			return nil, errors.New("could not read vendor/vendor.json")
		}
		var lockfile govendorLockfile
		if err := json.Unmarshal(lockfileContents, &lockfile); err != nil {
			return nil, errors.New("could not parse vendor/vendor.json")
		}
		for _, dependency := range lockfile.Package {
			lockfileVersions[dependency.Path] = dependency.Revision
		}
	}
	if ok, err := hasVndrManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `vndr` manifest\n")
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "vendor.conf"))
		if err != nil {
			return nil, errors.New("could not read vendor.conf")
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

	var resolvedDeps []module.Dependency
	// Strip out `/vendor/` weirdness in import paths.
	for _, dep := range deps {
		const vendorPrefix = "/vendor/"
		vendoredPathSections := strings.Split(dep.Package(), vendorPrefix)
		importPath := vendoredPathSections[len(vendoredPathSections)-1]
		// Note that `Version` is unset for import path tracing.
		resolvedDeps = append(resolvedDeps, GoPkg{ImportPath: importPath, Version: lockfileVersions[importPath]})
	}

	return resolvedDeps, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *GoBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	// Attempt to trace imports
	_, err := traceImports(m, allowUnresolved)
	if err != nil {
		return false, fmt.Errorf("could not trace gopkg imports: %s", err.Error())
	}
	goLogger.Debugf("IsBuilt tracing succeeded\n")

	// Find project folder
	projectFolder, hasProject, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return false, err
	}

	// Check for lockfiles
	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "Gopkg.lock"); err != nil || !ok {
			return false, err
		}
	}
	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "glide.lock"); err != nil || !ok {
			return false, err
		}
	}

	return hasProject, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *GoBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for GoBuilder")
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *GoBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for GoBuilder")
}
