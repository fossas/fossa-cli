package build

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
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
	ImportPath string `json:"name"`
	Version    string `json:"version"` // This is actually the Git revision, but `.Revision()` is already taken.

	isInternal bool
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

	// TODO: We can probably reduce the amount of `exec` and `os.Stat` calls we
	// make by caching results within private fields of `GoBuilder`.
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
	goLogger.Debugf("Found project folder for Build: %#v", projectFolder)

	// TODO: check and use builder.*Cmd
	if _, err := os.Stat(filepath.Join(projectFolder, "Gopkg.toml")); err == nil {
		goLogger.Debugf("Running `dep` build")
		if force {
			goLogger.Debugf("Clearing `dep` cache: `rm -rf vendor Gopkg.lock`...")
			cmd := exec.Command("rm", "-rf", "vendor", "Gopkg.lock")
			cmd.Dir = projectFolder
			output, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `dep` cache: %#v", err.Error())
			}
			goLogger.Debugf("...clearing `dep` cache done: %#v", output)
		}
		goLogger.Debugf("Running `dep ensure`...")
		cmd := exec.Command("dep", "ensure")
		cmd.Dir = projectFolder
		output, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `dep ensure`: %#v", err.Error())
		}
		goLogger.Debugf("...running `dep ensure` done: %#v", output)
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "glide.yaml")); err == nil {
		goLogger.Debugf("Running `glide` build")
		if force {
			cmd := exec.Command("rm", "-rf", "vendor", "glide.lock")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `glide` cache: %#v", err.Error())
			}
		}
		cmd := exec.Command("glide", "install")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `glide install`: %#v", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "Godeps/Godeps.json")); err == nil {
		goLogger.Debugf("Running `godep` build")
		if force {
			cmd := exec.Command("rm", "-rf", "vendor", "Godeps")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `godep` cache: %#v", err.Error())
			}
		}
		goLogger.Debugf("Running `godep save`...")
		cmd := exec.Command("godep", "save")
		cmd.Dir = projectFolder
		output, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `godep save`: %#v", err.Error())
		}
		goLogger.Debugf("...running `godep save` done: %#v", output)
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "vendor/vendor.json")); err == nil {
		goLogger.Debugf("Running `govendor` build")
		if force {
			backupCmd := exec.Command("mv", "vendor/vendor.json", "vendor.json.bak")
			backupCmd.Dir = projectFolder
			_, err := backupCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %#v", err.Error())
			}
			cleanCmd := exec.Command("rm", "-rf", "vendor")
			cleanCmd.Dir = projectFolder
			_, err = cleanCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %#v", err.Error())
			}
			makeFolderCmd := exec.Command("mkdir", "-p", "vendor")
			makeFolderCmd.Dir = projectFolder
			_, err = makeFolderCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %#v", err.Error())
			}
			restoreCmd := exec.Command("mv", "vendor.json.bak", "vendor/vendor.json")
			restoreCmd.Dir = projectFolder
			_, err = restoreCmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `govendor` cache: %#v", err.Error())
			}
		}
		cmd := exec.Command("govendor", "sync")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `govendor sync`: %#v", err.Error())
		}
	}

	if _, err := os.Stat(filepath.Join(projectFolder, "vendor.conf")); err == nil {
		goLogger.Debugf("Running `vndr` build")
		if force {
			cmd := exec.Command("rm", "-rf", "vendor")
			cmd.Dir = projectFolder
			_, err := cmd.Output()
			if err != nil {
				return fmt.Errorf("could not clear `vndr` cache: %#v", err.Error())
			}
		}
		cmd := exec.Command("vndr")
		cmd.Dir = projectFolder
		_, err := cmd.Output()
		if err != nil {
			return fmt.Errorf("could not run `vndr`: %#v", err.Error())
		}
	}

	return nil
}

// Build a dependency list given an entry point.
func traceImports(m module.Module, allowUnresolved bool) ([]GoPkg, error) {
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
func flattenDepTree(pkg depth.Pkg, allowUnresolved bool) ([]GoPkg, error) {
	var deps []GoPkg
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
			return []GoPkg{}, nil
		}
		return nil, errors.New("dependency of stdlib detected (this should never happen)")
	}

	if allowUnresolved || pkg.Resolved {
		return append(deps, GoPkg{ImportPath: pkg.Name, isInternal: pkg.Internal}), nil
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

func findProject(projects map[string]string, importPath string) (string, error) {
	project, err := findProjectRecurse(projects, importPath)
	if err != nil {
		goLogger.Debugf("Could not find project for import path %#v", importPath)
	} else {
		goLogger.Debugf("Found project %#v for import path %#v", project, importPath)
	}
	return project, err
}

func findProjectRecurse(projects map[string]string, importPath string) (string, error) {
	if importPath == "." {
		return "", fmt.Errorf("could not find project of import %#v", importPath)
	}
	_, ok := projects[importPath]
	if ok {
		return importPath, nil
	}
	return findProjectRecurse(projects, path.Dir(importPath))
}

func (builder *GoBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	// Trace imports
	deps, err := traceImports(m, allowUnresolved)
	if err != nil {
		return nil, fmt.Errorf("could not trace imports during analysis: %#v", err.Error())
	}
	goLogger.Debugf("Analyze tracing succeeded")

	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return nil, err
	}
	if !ok {
		goLogger.Debugf("Could not find project folder")
		if allowUnresolved {
			return nil, err
		}
		return nil, errors.New("could not find project folder")
	}
	goLogger.Debugf("Found project folder: %#v", projectFolder)

	// If possible, read lockfiles for versions
	lockfileVersions := make(map[string]string)

	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found `dep` manifest")
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
		goLogger.Debugf("Found `glide` manifest")
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
		goLogger.Debugf("Found `Godeps` manifest")
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
		goLogger.Debugf("Found `govendor` manifest")
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
		goLogger.Debugf("Found `vndr` manifest")
		lockfileContents, err := ioutil.ReadFile(filepath.Join(projectFolder, "vendor.conf"))
		if err != nil {
			return nil, errors.New("could not read vendor.conf")
		}
		lines := strings.Split(string(lockfileContents), "")
		for _, line := range lines {
			trimmedLine := strings.TrimSpace(line)
			if len(trimmedLine) > 0 && trimmedLine[0] != '#' {
				sections := strings.Split(trimmedLine, " ")
				lockfileVersions[sections[0]] = sections[1]
			}
		}
	}
	goLogger.Debugf("Read lockfile: %#v", lockfileVersions)

	depSet := make(map[GoPkg]bool)
	projectImports := strings.TrimPrefix(projectFolder, filepath.Join(os.Getenv("GOPATH"), "src")+string(filepath.Separator))
	for _, dep := range deps {
		// Strip out `/vendor/` weirdness in import paths.
		const vendorPrefix = "/vendor/"
		vendoredPathSections := strings.Split(dep.ImportPath, vendorPrefix)
		importPath := vendoredPathSections[len(vendoredPathSections)-1]
		// Get revisions (often these are scoped to repository, not package)
		project, err := findProject(lockfileVersions, importPath)
		if err != nil {
			if allowUnresolved ||
				dep.isInternal ||
				strings.Index(dep.ImportPath, projectImports) == 0 ||
				dep.ImportPath == "C" {
				goLogger.Warningf("Could not resolve import: %#v", dep)
				depSet[GoPkg{ImportPath: importPath, Version: ""}] = true
			} else {
				goLogger.Debugf("Could not resolve import: %#v", dep)
				goLogger.Debugf("Project folder: %#v", projectFolder)
				goLogger.Debugf("$GOPATH: %#v", os.Getenv("GOPATH"))
				goLogger.Debugf("Project folder relative to $GOPATH: %#v", projectImports)
				return nil, fmt.Errorf("could not resolve import: %#v", dep.ImportPath)
			}
		} else {
			depSet[GoPkg{ImportPath: project, Version: lockfileVersions[project]}] = true
		}
	}

	var resolvedDeps []module.Dependency
	for goPkg, ok := range depSet {
		if ok {
			resolvedDeps = append(resolvedDeps, goPkg)
		}
	}

	return resolvedDeps, nil
}

// IsBuilt checks whether dependencies are ready for scanning.
func (builder *GoBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	goLogger.Debugf("IsBuilt tracing start")
	// Attempt to trace imports
	_, err := traceImports(m, allowUnresolved)
	if err != nil {
		return false, fmt.Errorf("could not trace gopkg imports: %#v", err.Error())
	}
	goLogger.Debugf("IsBuilt tracing succeeded")

	// Find project folder
	projectFolder, hasProject, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return false, err
	}
	if !hasProject {
		goLogger.Debugf("IsBuilt failed: no project found")
		return false, nil
	}

	// Check for lockfiles
	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "Gopkg.lock"); err != nil || !ok {
			goLogger.Debugf("IsBuilt failed: `dep` manifest found, but no lockfile")
			return false, err
		}
	}
	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "glide.lock"); err != nil || !ok {
			goLogger.Debugf("IsBuilt failed: `glide` manifest found, but no lockfile")
			return false, err
		}
	}

	// Check for vendored dependencies
	if ok, err := hasFile(projectFolder, "vendor"); err != nil || !ok {
		goLogger.Debugf("IsBuilt failed: no `vendor` folder found")
		return false, err
	}

	return true, nil
}

func (builder *GoBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for GoBuilder")
}

func (builder *GoBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for GoBuilder")
}
