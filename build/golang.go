package build

import (
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
	Version    string `json:"version"` // This is actually the Git revision, but `.Revision()` is already taken by Dependency.

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

// Initialize collects metadata on Go, Dep, Glide, Godep, Govendor, and Vndr binaries.
func (builder *GoBuilder) Initialize() error {
	goLogger.Debug("Initializing Go builder...")

	// Set Go context variables
	goCmd, goVersion, err := which("version", os.Getenv("GO_BINARY"), "go")
	if err != nil {
		return fmt.Errorf("could not find Go binary (try setting $GO_BINARY): %s", err.Error())
	}
	builder.GoCmd = goCmd
	builder.GoVersion = goVersion

	// Set Dep context variables
	depCmd, depVersion, depErr := which("version", os.Getenv("DEP_BINARY"), "dep")
	builder.DepCmd = depCmd
	builder.DepVersion = depVersion

	// Set Glide context variables
	glideCmd, glideVersion, glideErr := which("-v", os.Getenv("GLIDE_BINARY"), "glide")
	builder.GlideCmd = glideCmd
	builder.GlideVersion = glideVersion

	// Set Godep context variables
	godepCmd, godepVersion, godepErr := which("version", os.Getenv("GODEP_BINARY"), "godep")
	builder.GodepCmd = godepCmd
	builder.GodepVersion = godepVersion

	// Set Govendor context variables
	govendorCmd, govendorVersion, govendorErr := which("--version", os.Getenv("GOVENDOR_BINARY"), "govendor")
	builder.GovendorCmd = govendorCmd
	builder.GovendorVersion = govendorVersion

	// Set vndr context variables
	// NOTE: vndr doesn't have a version flag and exits with code 1 on `--help`
	vndrCmd, _, vndrErr := whichWithResolver([]string{os.Getenv("VNDR_BINARY"), "vndr"}, func(cmd string) (string, error) {
		_, _, err := run(cmd, "--help")
		_, isExitError := err.(*exec.ExitError)
		if err != nil && !isExitError {
			return "", err
		}
		return "", nil
	})
	builder.VndrCmd = vndrCmd
	builder.VndrVersion = ""

	if depErr != nil && glideErr != nil && godepErr != nil && govendorErr != nil && vndrErr != nil {
		return fmt.Errorf("no supported Go build tools detected (try setting $DEP_BINARY or $GLIDE_BINARY or $GODEP_BINARY or $GOVENDOR_BINARY or $VNDR_BINARY): %#v %#v %#v %#v %#v", depErr, glideErr, godepErr, govendorErr, vndrErr)
	}

	goLogger.Debugf("Done initializing Go builder: %#v", builder)
	return nil
}

// Helpers for finding a Go project folder
func hasDepManifest(path string) (bool, error) {
	return hasFile(path, "Gopkg.toml")
}

func hasGlideManifest(path string) (bool, error) {
	return hasFile(path, "glide.yaml")
}

func hasGodepManifest(path string) (bool, error) {
	return hasFile(path, "Godeps", "Godeps.json")
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

// Helper for running most Go build tools
func runGoTool(projectFolder string, hasManifest fileChecker, buildCmd string, cleanCmd string, force bool) error {
	cleanCmds := strings.Split(cleanCmd, " ")
	return runGoToolWithCleaner(projectFolder, hasManifest, buildCmd, force, func() error {
		_, _, err := runLogged(goLogger, projectFolder, cleanCmds[0], cleanCmds[1:]...)
		return err
	})
}

type goToolCleaner func() error

func runGoToolWithCleaner(projectFolder string, hasManifest fileChecker, buildCmd string, force bool, cleaner goToolCleaner) error {
	buildCmds := strings.Split(buildCmd, " ")
	toolName := strings.Title(buildCmds[0])

	if ok, err := hasManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found %s manifest: running %s build", toolName, toolName)

		if force {
			err := cleaner()
			if err != nil {
				return fmt.Errorf("could not remove %s cache: %s", toolName, err.Error())
			}
		}
		_, _, err := runLogged(goLogger, projectFolder, buildCmds[0], buildCmds[1:]...)
		if err != nil {
			return fmt.Errorf("could not run %s build: %s", toolName, err.Error())
		}
	}

	return nil
}

// Build contextually runs a build tool
func (builder *GoBuilder) Build(m module.Module, force bool) error {
	goLogger.Debugf("Running Go build: %#v %#v", m, force)

	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return fmt.Errorf("could not find Go project folder: %s", err.Error())
	}
	if !ok {
		return errors.New("could not find Go project folder (maybe your Go build tool is not supported?)")
	}
	goLogger.Debugf("Found project folder for Go build: %#v", projectFolder)

	// Run build tools
	err = runGoTool(projectFolder, hasDepManifest, "dep ensure", "rm -rf vendor Gopkg.lock", force)
	if err != nil {
		return err
	}

	err = runGoTool(projectFolder, hasGlideManifest, "glide install", "rm -rf vendor glide.yaml", force)
	if err != nil {
		return err
	}

	err = runGoTool(projectFolder, hasGodepManifest, "godep restore", "rm -rf vendor Godeps", force)
	if err != nil {
		return err
	}

	// Govendor is handled differently because it stores its manifest _within_ the vendor folder
	err = runGoToolWithCleaner(projectFolder, hasGovendorManifest, "govendor sync", force, func() error {
		_, _, err := runLogged(goLogger, projectFolder, "mv", "vendor/vendor.json", "vendor.json.bak")
		if err != nil {
			return err
		}
		_, _, err = runLogged(goLogger, projectFolder, "rm", "-rf", "vendor")
		if err != nil {
			return err
		}
		_, _, err = runLogged(goLogger, projectFolder, "mkdir", "-p", "vendor")
		if err != nil {
			return err
		}
		_, _, err = runLogged(goLogger, projectFolder, "mv", "vendor.json.bak", "vendor/vendor.json")
		if err != nil {
			return err
		}
		return nil
	})

	err = runGoTool(projectFolder, hasVndrManifest, "vndr", "rm -rf vendor", force)
	if err != nil {
		return err
	}

	goLogger.Debugf("Done running Go build.")
	return nil
}

// Build a dependency list given an entry point.
func traceImports(m module.Module, allowUnresolved bool) ([]GoPkg, error) {
	// Do import tracing.
	var tree depth.Tree
	err := tree.Resolve(m.Target)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}

	// Flatten tree (technically a DAG?) into a list.
	deps, err := flattenGoDeps(*tree.Root, allowUnresolved)
	if err != nil {
		return nil, errors.New("could not resolve dependencies: " + err.Error())
	}

	return deps, nil
}

// Recursively flatten the dependency tree.
func flattenGoDeps(pkg depth.Pkg, allowUnresolved bool) ([]GoPkg, error) {
	// Ignore "internal" (i.e. standard library) packages.
	if pkg.Internal {
		if len(pkg.Deps) == 0 {
			return []GoPkg{GoPkg{ImportPath: pkg.Name, isInternal: pkg.Internal}}, nil
		}
		return nil, errors.New("dependency of stdlib detected (this should never happen)")
	}

	// Get recursively flattened trees of child dependencies.
	var deps []GoPkg
	for _, dep := range pkg.Deps {
		flattened, err := flattenGoDeps(dep, allowUnresolved)
		if err != nil {
			return nil, err
		}
		deps = append(deps, flattened...)
	}

	// Add this package if it's resolved.
	if allowUnresolved || pkg.Resolved {
		return append(deps, GoPkg{ImportPath: pkg.Name, isInternal: pkg.Internal}), nil
	}

	// Otherwise, fail.
	return nil, errors.New("could not resolve package: " + pkg.Name)
}

// Lockfile structs for JSON unmarshalling
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

// Helpers for resolving the project revision of an import
func findRevision(projects map[string]string, importPath string) (string, error) {
	project, err := findRevisionRecurse(projects, importPath)
	if err != nil {
		goLogger.Debugf("Could not find project for import path %#v", importPath)
	} else {
		goLogger.Debugf("Found project %#v for import path %#v", project, importPath)
	}
	return project, err
}

func findRevisionRecurse(projects map[string]string, importPath string) (string, error) {
	if importPath == "." {
		return "", fmt.Errorf("could not find project of import %#v", importPath)
	}
	_, ok := projects[importPath]
	if ok {
		return importPath, nil
	}
	return findRevisionRecurse(projects, path.Dir(importPath))
}

// Analyze traces imports and then looks up revisions in lockfiles
func (builder *GoBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	goLogger.Debugf("Running Go analysis: %#v %#v", m, allowUnresolved)

	// Trace imports
	traced, err := traceImports(m, allowUnresolved)
	if err != nil {
		return nil, fmt.Errorf("could not trace go imports: %#v", err.Error())
	}
	goLogger.Debugf("Traced imports: %#v", traced)

	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return nil, err
	}
	if !ok {
		goLogger.Debugf("Could not find project folder")

		if allowUnresolved {
			var deps []module.Dependency
			for _, dep := range traced {
				deps = append(deps, dep)
			}
			return deps, err
		}
		return nil, errors.New("could not find project folder")
	}
	goLogger.Debugf("Found project folder: %#v", projectFolder)

	// If possible, read lockfiles for versions
	lockfileVersions := make(map[string]string)

	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found Dep manifest")

		var lockfile depLockfile
		parseLoggedWithUnmarshaller(goLogger, filepath.Join(projectFolder, "Gopkg.lock"), &lockfile, func(data []byte, v interface{}) error {
			_, err := toml.Decode(string(data), v)
			return err
		})
		for _, dependency := range lockfile.Projects {
			lockfileVersions[dependency.Name] = dependency.Revision
		}

		goLogger.Debugf("Parsed Dep manifest: %#v", lockfile.Projects)
	}

	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found Glide manifest")

		var lockfile glideLockfile
		parseLoggedWithUnmarshaller(goLogger, filepath.Join(projectFolder, "glide.lock"), &lockfile, yaml.Unmarshal)
		for _, dependency := range lockfile.Imports {
			lockfileVersions[dependency.Name] = dependency.Version
		}

		goLogger.Debugf("Parsed Glide manifest: %#v", lockfile.Imports)
	}

	if ok, err := hasGodepManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found Godeps manifest")

		var lockfile godepLockfile
		parseLogged(goLogger, filepath.Join(projectFolder, "Godeps", "Godeps.json"), &lockfile)
		for _, dependency := range lockfile.Deps {
			lockfileVersions[dependency.ImportPath] = dependency.Rev
		}

		goLogger.Debugf("Parsed Godeps manifest: %#v", lockfile.Deps)
	}

	if ok, err := hasGovendorManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found Govendor manifest")

		var lockfile govendorLockfile
		parseLogged(goLogger, filepath.Join(projectFolder, "vendor", "vendor.json"), &lockfile)
		for _, dependency := range lockfile.Package {
			lockfileVersions[dependency.Path] = dependency.Revision
		}

		goLogger.Debugf("Parsed Godeps manifest: %#v", lockfile.Package)
	}

	// vndr is the odd one out because it rolls its own format
	if ok, err := hasVndrManifest(projectFolder); err == nil && ok {
		goLogger.Debugf("Found Vndr manifest")

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

		goLogger.Debugf("Parsed vndr manifest: %#v", lockfileVersions)
	}
	goLogger.Debugf("Parsed lockfiles: %#v", lockfileVersions)

	depSet := make(map[GoPkg]bool)
	projectImports := strings.TrimPrefix(projectFolder, filepath.Join(os.Getenv("GOPATH"), "src")+string(filepath.Separator))
	for _, dep := range traced {
		// Strip out `/vendor/` weirdness in import paths.
		const vendorPrefix = "/vendor/"
		vendoredPathSections := strings.Split(dep.ImportPath, vendorPrefix)
		importPath := vendoredPathSections[len(vendoredPathSections)-1]

		// Get revisions (often these are scoped to repository, not package)
		project, err := findRevision(lockfileVersions, importPath)
		if err != nil {
			if dep.isInternal ||
				strings.Index(importPath, projectImports) == 0 ||
				dep.ImportPath == "C" {
				goLogger.Debugf("Did not resolve import: %#v", dep)
				depSet[GoPkg{ImportPath: importPath, Version: ""}] = true
			} else if allowUnresolved {
				goLogger.Warningf("Could not resolve import: %#v", dep)
				depSet[GoPkg{ImportPath: importPath, Version: ""}] = true
			} else {
				goLogger.Errorf("Could not resolve import: %#v", dep)
				goLogger.Debugf("Project folder: %#v", projectFolder)
				goLogger.Debugf("$GOPATH: %#v", os.Getenv("GOPATH"))
				goLogger.Debugf("Project folder relative to $GOPATH: %#v", projectImports)
				goLogger.Debugf("Lockfile versions: %#v", lockfileVersions)
				return nil, fmt.Errorf("could not resolve import: %#v", dep.ImportPath)
			}
		} else {
			depSet[GoPkg{ImportPath: project, Version: lockfileVersions[project]}] = true
		}
	}

	var deps []module.Dependency
	for goPkg, ok := range depSet {
		if ok {
			deps = append(deps, goPkg)
		}
	}

	goLogger.Debugf("Done running Go analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether imports are traceable and lockfiles are available
func (builder *GoBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	goLogger.Debugf("Checking Go build: %#v %#v", m, allowUnresolved)

	// Attempt to trace imports
	_, err := traceImports(m, allowUnresolved)
	if err != nil {
		return false, fmt.Errorf("could not trace go imports: %s", err.Error())
	}
	goLogger.Debugf("Tracing imports OK")

	// Find project folder
	projectFolder, hasProject, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return false, fmt.Errorf("could not find project folder: %s", err.Error())
	}
	if !hasProject {
		goLogger.Debugf("Checking Go build failed: no project found")
		return false, nil
	}
	goLogger.Debugf("Project folder OK")

	// Check for lockfiles
	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "Gopkg.lock"); err != nil || !ok {
			goLogger.Debugf("Checking Go build failed: Dep manifest found, but no lockfile")
			return false, err
		}
	}
	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "glide.lock"); err != nil || !ok {
			goLogger.Debugf("Checking Go build failed: Glide manifest found, but no lockfile")
			return false, err
		}
	}

	// Check for vendored dependencies
	if ok, err := hasFile(projectFolder, "vendor"); err != nil || !ok {
		goLogger.Debugf("Checking Go build failed: no `vendor` folder found")
		return false, err
	}

	goLogger.Debugf("Done checking Go build: %#v", true)
	return true, nil
}

// IsModule is not implemented
func (builder *GoBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for GoBuilder")
}

// InferModule is not implemented
func (builder *GoBuilder) InferModule(target string) (module.Module, error) {
	return module.Module{}, errors.New("InferModule is not implemented for GoBuilder")
}
