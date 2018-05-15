package builders

import (
	"errors"
	"fmt"
	"go/parser"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"

	"github.com/BurntSushi/toml"
	yaml "gopkg.in/yaml.v2"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
)

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

	// gdm
	GdmCmd     string
	GdmVersion string

	// TODO: `gpm` support?
	// TODO: `gvt` support?

	// TODO: We can probably reduce the amount of `exec` and `os.Stat` calls we
	// make by caching results within private fields of `GoBuilder`.
}

// Initialize collects metadata on Go, Dep, Glide, Godep, Govendor, and Vndr binaries.
func (builder *GoBuilder) Initialize() error {
	log.Debug("Initializing Go builder...")

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
		log.Warningf("No supported Go build tools detected (try setting $DEP_BINARY or $GLIDE_BINARY or $GODEP_BINARY or $GOVENDOR_BINARY or $VNDR_BINARY): %#v %#v %#v %#v %#v", depErr, glideErr, godepErr, govendorErr, vndrErr)
	}

	log.Debugf("Done initializing Go builder: %#v", builder)
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
	ok, err := isFolder(path, "Godeps")
	if err != nil {
		return false, err
	}
	if !ok {
		return false, nil
	}
	return hasFile(path, "Godeps", "Godeps.json")
}

func hasGovendorManifest(path string) (bool, error) {
	return hasFile(path, "vendor", "vendor.json")
}

func hasVndrManifest(path string) (bool, error) {
	return hasFile(path, "vendor.conf")
}

func hasGdmManifest(path string) (bool, error) {
	return isFile(path, "Godeps")
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
		_, _, err := runLogged(projectFolder, cleanCmds[0], cleanCmds[1:]...)
		return err
	})
}

type goToolCleaner func() error

func runGoToolWithCleaner(projectFolder string, hasManifest fileChecker, buildCmd string, force bool, cleaner goToolCleaner) error {
	buildCmds := strings.Split(buildCmd, " ")
	toolName := strings.Title(buildCmds[0])

	if ok, err := hasManifest(projectFolder); err == nil && ok {
		log.Debugf("Found %s manifest: running %s build", toolName, toolName)

		if force {
			err := cleaner()
			if err != nil {
				return fmt.Errorf("could not remove %s cache: %s", toolName, err.Error())
			}
		}
		_, _, err := runLogged(projectFolder, buildCmds[0], buildCmds[1:]...)
		if err != nil {
			return fmt.Errorf("could not run %s build: %s", toolName, err.Error())
		}
	}

	return nil
}

// Build contextually runs a build tool
func (builder *GoBuilder) Build(m module.Module, force bool) error {
	log.Debugf("Running Go build: %#v %#v", m, force)

	// Find project folder (this is an ancestor of the module folder)
	projectFolder, ok, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return fmt.Errorf("could not find Go project folder: %s", err.Error())
	}
	if !ok {
		log.Warningf("Could not find Go project folder (maybe your Go build tool is not supported?)")
	}
	log.Debugf("Found project folder for Go build: %#v", projectFolder)

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
		_, _, err := runLogged(projectFolder, "mv", "vendor/vendor.json", "vendor.json.bak")
		if err != nil {
			return err
		}
		_, _, err = runLogged(projectFolder, "rm", "-rf", "vendor")
		if err != nil {
			return err
		}
		_, _, err = runLogged(projectFolder, "mkdir", "-p", "vendor")
		if err != nil {
			return err
		}
		_, _, err = runLogged(projectFolder, "mv", "vendor.json.bak", "vendor/vendor.json")
		if err != nil {
			return err
		}
		return nil
	})

	err = runGoTool(projectFolder, hasVndrManifest, "vndr", "rm -rf vendor", force)
	if err != nil {
		return err
	}

	err = runGoTool(projectFolder, hasGdmManifest, "gdm vendor", "rm -rf vendor", force)
	if err != nil {
		return err
	}

	// Run an actual Go build
	_, _, err = runLogged(m.Dir, "go", "build", "./...")
	if err != nil {
		return err
	}

	log.Debugf("Done running Go build.")
	return nil
}

var goInternalPackages = map[string]bool{
	"C":                    true,
	"archive":              true,
	"archive/tar":          true,
	"archive/zip":          true,
	"bufio":                true,
	"builtin":              true,
	"bytes":                true,
	"compress":             true,
	"compress/bzip2":       true,
	"compress/flate":       true,
	"compress/gzip":        true,
	"compress/lzw":         true,
	"compress/zlib":        true,
	"container":            true,
	"container/heap":       true,
	"container/list":       true,
	"container/ring":       true,
	"context":              true,
	"crypto":               true,
	"crypto/aes":           true,
	"crypto/cipher":        true,
	"crypto/des":           true,
	"crypto/dsa":           true,
	"crypto/ecdsa":         true,
	"crypto/elliptic":      true,
	"crypto/hmac":          true,
	"crypto/md5":           true,
	"crypto/rand":          true,
	"crypto/rc4":           true,
	"crypto/rsa":           true,
	"crypto/sha1":          true,
	"crypto/sha256":        true,
	"crypto/sha512":        true,
	"crypto/subtle":        true,
	"crypto/tls":           true,
	"crypto/x509":          true,
	"crypto/x509/pkix":     true,
	"database":             true,
	"database/sql":         true,
	"database/sql/driver":  true,
	"debug":                true,
	"debug/dwarf":          true,
	"debug/elf":            true,
	"debug/gosym":          true,
	"debug/macho":          true,
	"debug/pe":             true,
	"debug/plan9obj":       true,
	"encoding":             true,
	"encoding/ascii85":     true,
	"encoding/asn1":        true,
	"encoding/base32":      true,
	"encoding/base64":      true,
	"encoding/binary":      true,
	"encoding/csv":         true,
	"encoding/gob":         true,
	"encoding/hex":         true,
	"encoding/json":        true,
	"encoding/pem":         true,
	"encoding/xml":         true,
	"errors":               true,
	"expvar":               true,
	"flag":                 true,
	"fmt":                  true,
	"go":                   true,
	"go/ast":               true,
	"go/build":             true,
	"go/constant":          true,
	"go/doc":               true,
	"go/format":            true,
	"go/importer":          true,
	"go/parser":            true,
	"go/printer":           true,
	"go/scanner":           true,
	"go/token":             true,
	"go/types":             true,
	"hash":                 true,
	"hash/adler32":         true,
	"hash/crc32":           true,
	"hash/crc64":           true,
	"hash/fnv":             true,
	"html":                 true,
	"html/template":        true,
	"image":                true,
	"image/color":          true,
	"image/color/palette":  true,
	"image/draw":           true,
	"image/gif":            true,
	"image/jpeg":           true,
	"image/png":            true,
	"index":                true,
	"index/suffixarray":    true,
	"io":                   true,
	"io/ioutil":            true,
	"log":                  true,
	"log/syslog":           true,
	"math":                 true,
	"math/big":             true,
	"math/bits":            true,
	"math/cmplx":           true,
	"math/rand":            true,
	"mime":                 true,
	"mime/multipart":       true,
	"mime/quotedprintable": true,
	"net":                 true,
	"net/http":            true,
	"net/http/cgi":        true,
	"net/http/cookiejar":  true,
	"net/http/fcgi":       true,
	"net/http/httptest":   true,
	"net/http/httptrace":  true,
	"net/http/httputil":   true,
	"net/http/pprof":      true,
	"net/mail":            true,
	"net/rpc":             true,
	"net/rpc/jsonrpc":     true,
	"net/smtp":            true,
	"net/textproto":       true,
	"net/url":             true,
	"os":                  true,
	"os/exec":             true,
	"os/signal":           true,
	"os/user":             true,
	"path":                true,
	"path/filepath":       true,
	"plugin":              true,
	"reflect":             true,
	"regexp":              true,
	"regexp/syntax":       true,
	"runtime":             true,
	"runtime/cgo":         true,
	"runtime/debug":       true,
	"runtime/msan":        true,
	"runtime/pprof":       true,
	"runtime/race":        true,
	"runtime/trace":       true,
	"sort":                true,
	"strconv":             true,
	"strings":             true,
	"sync":                true,
	"sync/atomic":         true,
	"syscall":             true,
	"testing":             true,
	"testing/iotest":      true,
	"testing/quick":       true,
	"text":                true,
	"text/scanner":        true,
	"text/tabwriter":      true,
	"text/template":       true,
	"text/template/parse": true,
	"time":                true,
	"unicode":             true,
	"unicode/utf16":       true,
	"unicode/utf8":        true,
	"unsafe":              true,
}

type goPkg struct {
	ImportPath string
	Revision   string

	isInternal bool
}

func goImportIsInternal(pkg string) bool {
	if pkg == "." {
		return false
	}
	// TEST: Standard library packages + packages labelled "internal" won't have
	// resolved versions in the lockfile.
	if goInternalPackages[pkg] || strings.Index(pkg, "internal") != -1 {
		return true
	}
	// TEST: This is for packages like `crypto/internal/cipherhw`
	return goImportIsInternal(path.Dir(pkg))
}

// NOTE: we don't really need the module.Module argument, that's just a hack so I can use runLogged easily.
func getGoImportsRecurse(builder *GoBuilder, m module.Module, memo map[string]string, from module.ImportPath, pkg string) ([]Imported, error) {
	if goImportIsInternal(pkg) {
		return []Imported{}, nil
	}

	stdout, ok := memo[pkg]
	if !ok {
		var err error
		stdout, _, err = runLogged(m.Dir, builder.GoCmd, "list", "-f", "{{ join .Imports \"\\n\" }}", pkg)
		if err != nil {
			return nil, fmt.Errorf("could not trace imports: %s", err.Error())
		}
		memo[pkg] = stdout
	}

	locator := module.Locator{
		Fetcher:  "go",
		Project:  pkg,
		Revision: "",
	}
	var imports []Imported
	for _, dep := range strings.Split(stdout, "\n") {
		if dep == "" {
			continue
		}
		transitive, err := getGoImportsRecurse(builder, m, memo, append(from, locator), dep)
		if err != nil {
			return nil, err
		}
		imports = append(imports, transitive...)
	}
	imports = append(imports, Imported{
		Locator: locator,
		From:    append(module.ImportPath{}, from...),
	})

	return imports, nil
}

// Build a dependency list given an entry point.
func getGoImports(builder *GoBuilder, m module.Module) ([]Imported, error) {
	imports, err := getGoImportsRecurse(
		builder,
		m,
		make(map[string]string),
		nil,
		m.Target,
	)
	// TEST: imports should not include the root importing package
	return imports[:len(imports)-1], err
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

// Lockfile parsers for common "simple" formats
func parseGPMLockfile(lockfileVersions *map[string]string, path ...string) error {
	lockfile := filepath.Join(path...)
	lockfileContents, err := ioutil.ReadFile(lockfile)
	if err != nil {
		log.Debugf("Error reading %s: %s", lockfile, err.Error())
		return fmt.Errorf("could not read %s: %s", lockfile, err.Error())
	}

	lines := strings.Split(string(lockfileContents), "\n")
	for _, line := range lines {
		trimmedLine := strings.TrimSpace(line)
		if len(trimmedLine) > 0 && trimmedLine[0] != '#' {
			sections := strings.Split(trimmedLine, " ")
			(*lockfileVersions)[sections[0]] = sections[1]
		}
	}

	return nil
}

// Helpers for resolving the project revision of an import
func findRevision(projects map[string]string, importPath string) (string, error) {
	project, err := findRevisionRecurse(projects, importPath)
	if err != nil {
		log.Debugf("Could not find project for import path %#v", importPath)
	} else {
		log.Debugf("Found project %#v for import path %#v", project, importPath)
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

var errNoLockfile = errors.New("could not find lockfile")

// TODO: there might actually be a more sane way of doing this: search upwards
// in the import path for every /vendor/ folder, which should accompany every
// package manifest (unless, of course, you're using legacy Godeps or another
// import path rewriting tool...)
func readLockfile(dir string) (map[string]string, error) {
	// If possible, read lockfiles for versions
	lockfileVersions := make(map[string]string)

	if ok, err := hasDepManifest(dir); err == nil && ok {
		log.Debugf("Found Dep manifest")

		var lockfile depLockfile
		parseLoggedWithUnmarshaller(filepath.Join(dir, "Gopkg.lock"), &lockfile, func(data []byte, v interface{}) error {
			_, err := toml.Decode(string(data), v)
			return err
		})
		for _, dependency := range lockfile.Projects {
			lockfileVersions[dependency.Name] = dependency.Revision
		}

		log.Debugf("Parsed Dep manifest: %#v", lockfile.Projects)
	}

	if ok, err := hasGlideManifest(dir); err == nil && ok {
		log.Debugf("Found Glide manifest")

		var lockfile glideLockfile
		parseLoggedWithUnmarshaller(filepath.Join(dir, "glide.lock"), &lockfile, yaml.Unmarshal)
		for _, dependency := range lockfile.Imports {
			lockfileVersions[dependency.Name] = dependency.Version
		}

		log.Debugf("Parsed Glide manifest: %#v", lockfile.Imports)
	}

	if ok, err := hasGodepManifest(dir); err == nil && ok {
		log.Debugf("Found Godeps manifest")

		var lockfile godepLockfile
		parseLogged(filepath.Join(dir, "Godeps", "Godeps.json"), &lockfile)
		for _, dependency := range lockfile.Deps {
			lockfileVersions[dependency.ImportPath] = dependency.Rev
		}

		log.Debugf("Parsed Godeps manifest: %#v", lockfile.Deps)
	}

	if ok, err := hasGovendorManifest(dir); err == nil && ok {
		log.Debugf("Found Govendor manifest")

		var lockfile govendorLockfile
		parseLogged(filepath.Join(dir, "vendor", "vendor.json"), &lockfile)
		for _, dependency := range lockfile.Package {
			lockfileVersions[dependency.Path] = dependency.Revision
		}

		log.Debugf("Parsed Godeps manifest: %#v", lockfile.Package)
	}

	if ok, err := hasVndrManifest(dir); err == nil && ok {
		log.Debugf("Found Vndr manifest")

		parseGPMLockfile(&lockfileVersions, dir, "vendor.conf")

		log.Debugf("Parsed Vndr manifest: %#v", lockfileVersions)
	}

	// gdm rolls its own format as well
	if ok, err := hasGdmManifest(dir); err == nil && ok {
		log.Debugf("Found Gdm manifest")

		parseGPMLockfile(&lockfileVersions, dir, "Godeps")

		log.Debugf("Parsed Gndr manifest: %#v", lockfileVersions)
	}

	return lockfileVersions, nil
}

func goImportToDir(pkg string) string {
	return filepath.Join(os.Getenv("GOPATH"), "src", pkg)
}

// Analyze traces imports and then looks up revisions in lockfiles
func (builder *GoBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	log.Debugf("Running Go analysis: %#v %#v", m, allowUnresolved)

	// Trace imports
	traced, err := getGoImports(builder, m)
	if err != nil {
		return nil, fmt.Errorf("could not trace go imports: %#v", err.Error())
	}
	log.Debugf("Traced imports: %#v", traced)

	// Resolve the version of each import by finding its appropriate lockfile and reading it.
	for i, pkg := range traced {
		// Get the project folder
		packageDir := goImportToDir(pkg.Project)
		// TEST: project revisions are only ever locked by _parents_ of the project,
		// not the project itself.
		// TODO: should we search through all possible ancestor lockfiles instead of
		// just the nearest one?
		projectFolder, ok, err := findGoProjectFolder(filepath.Dir(packageDir))
		if err != nil {
			return nil, err
		}
		if !ok {
			if allowUnresolved {
				log.Warningf("Could not find lockfile for package %s", pkg.Project)
				continue
			} else {
				return nil, err
			}
		}
		log.Debugf("Found project folder: %#v", projectFolder)

		// Get the lockfile
		lockfile, err := readLockfile(projectFolder)
		if err != nil {
			if allowUnresolved {
				log.Warningf("Could not find lockfile for package %s", pkg.Project)
				continue
			} else {
				return nil, err
			}
		}

		// Process the import path
		projectGopath := strings.TrimPrefix(projectFolder, filepath.Join(os.Getenv("GOPATH"), "src")+string(filepath.Separator))
		// Work around awful Go compiler hack: see https://github.com/golang/go/issues/16333
		if strings.HasPrefix(pkg.Project, "vendor/golang_org") {
			continue
		}

		// Strip `/vendor/` folders.
		const vendorPrefix = "/vendor/"
		vendoredPathSections := strings.Split(pkg.Project, vendorPrefix)
		importPath := vendoredPathSections[len(vendoredPathSections)-1]
		log.Debugf("Resolving import: %s", importPath)

		vendoredProjectSections := strings.Split(projectGopath, vendorPrefix)
		projectVendored := vendoredProjectSections[len(vendoredProjectSections)-1]

		importedProject, err := findRevision(lockfile, importPath)
		if err != nil {
			if strings.Index(importPath, projectGopath) == 0 || strings.Index(importPath, projectVendored) == 0 {
				log.Debugf("Did not resolve import: %#v", pkg.Project)
				traced[i].Revision = lockfile[importedProject]
			} else if allowUnresolved {
				log.Warningf("Could not resolve import: %#v", pkg.Project)
				traced[i].Revision = lockfile[importedProject]
			} else {
				log.Warningf("Could not resolve import: %#v", pkg.Project)
				log.Debugf("Project folder: %#v", projectFolder)
				log.Debugf("$GOPATH: %#v", os.Getenv("GOPATH"))
				log.Debugf("Project folder relative to $GOPATH: %#v", projectGopath)
				log.Debugf("Project folder relative to vendoring: %#v", projectVendored)
				log.Debugf("Lockfile versions: %#v", lockfile)
				return nil, fmt.Errorf("could not resolve import: %#v", importPath)
			}
		} else {
			traced[i].Revision = lockfile[importedProject]
		}
	}
	deps := computeImportPaths(traced)

	log.Debugf("Done running Go analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks whether imports are traceable and lockfiles are available
func (builder *GoBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	log.Debugf("Checking Go build: %#v %#v", m, allowUnresolved)

	// Attempt to trace imports
	_, err := getGoImports(builder, m)
	if err != nil {
		return false, fmt.Errorf("could not trace go imports: %s", err.Error())
	}
	log.Debugf("Tracing imports OK")

	if allowUnresolved {
		return true, nil
	}

	// Find project folder
	projectFolder, hasProject, err := findGoProjectFolder(m.Dir)
	if err != nil {
		return false, fmt.Errorf("could not find project folder: %s", err.Error())
	}
	if !hasProject {
		log.Debugf("Checking Go build failed: no project found")
		return false, nil
	}
	log.Debugf("Project folder OK")

	// Check for lockfiles
	if ok, err := hasDepManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "Gopkg.lock"); err != nil || !ok {
			log.Debugf("Checking Go build failed: Dep manifest found, but no lockfile")
			return false, err
		}
	}
	if ok, err := hasGlideManifest(projectFolder); err == nil && ok {
		if ok, err := hasFile(projectFolder, "glide.lock"); err != nil || !ok {
			log.Debugf("Checking Go build failed: Glide manifest found, but no lockfile")
			return false, err
		}
	}

	log.Debugf("Done checking Go build: %#v", true)
	return true, nil
}

// IsModule is not implemented
func (builder *GoBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for GoBuilder")
}

// DiscoverModules walks subdirectories for a Go file with `package main`.
func (builder *GoBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	var modules []module.Config
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Debugf("Failed to access path %s: %s", path, err.Error())
			return fmt.Errorf("could not read path %s during go module discovery: %s", path, err.Error())
		}
		// Skip files (we parse a directory at a time)
		if !info.IsDir() {
			return nil
		}
		// Skip vendor directories
		if info.Name() == "vendor" {
			log.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}
		// Parse directory, check for `main` package declaration.
		files := token.NewFileSet()
		pkgs, err := parser.ParseDir(files, path, nil, parser.PackageClauseOnly)
		if err != nil {
			return fmt.Errorf("could not parse directory %s during go module discovery: %s", path, err.Error())
		}
		for pkg := range pkgs {
			if pkg == "main" {
				modulePath, err := filepath.Rel(dir, path)
				if err != nil {
					return fmt.Errorf("could not compute module path: %s", err.Error())
				}
				modules = append(modules, module.Config{
					Name: info.Name(),
					Path: modulePath,
					Type: "go",
				})
			}
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("could not discover go modules: %s", err.Error())
	}
	return modules, nil
}
