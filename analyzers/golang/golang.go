// Package golang implements the analyzer for Go.
//
// This package is implemented by externally calling both the `go` tool and any
// external build tools.
//
// FAQ
//
// Why not use `go/build`, or a library like `KyleBanks/depth`?
//
// The `go` tool's interface is incredibly stable over different releases, but
// the internals are not. Using these libraries causes crashes when analyzing
// code that is compiled using a different version of Go. (This was how the
// analyzer was originally implemented.)
package golang

import (
	"os"
	"strings"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/errutil"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// An Analyzer contains structs used in the analysis of Go packages.
type Analyzer struct {
	Go        gocmd.Go
	GoVersion string

	Options Options
}

// Options set analyzer options for Go modules.
type Options struct {
	BuildOS               string `mapstructure:"os"`                      // Target build OS (for build tags).
	BuildArch             string `mapstructure:"arch"`                    // Target build architecture (for build tags).
	Strategy              string `mapstructure:"strategy"`                // See the Go analyzer documentation.
	LockfilePath          string `mapstructure:"lockfile"`                // For non-standard lockfile locations with strategies `manifest:*`.
	AllowUnresolved       bool   `mapstructure:"allow-unresolved"`        // Allow unresolved revisions.
	AllowUnresolvedPrefix string `mapstructure:"allow-unresolved-prefix"` // If set, allows unresolved revisions for packages whose import path's prefix matches.
	SkipImportTracing     bool   `mapstructure:"skip-tracing"`            // If true, skips dependency tracing.
	SkipProject           bool   `mapstructure:"skip-project"`            // If true, skips project detection.
}

// New constructs an Analyzer.
func New(opts map[string]interface{}) (*Analyzer, error) {
	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	// Construct analyzer.
	cmd, version, err := exec.Which("version", os.Getenv("FOSSA_GO_CMD"), "go")
	if err != nil {
		return nil, err
	}
	return &Analyzer{
		Go: gocmd.Go{
			Cmd:  cmd,
			OS:   options.BuildOS,
			Arch: options.BuildArch,
		},
		GoVersion: version,
		Options:   options,
	}, nil
}

// Discover runs `go list ./...`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	found, err := a.Go.List([]string{"./..."})
	if err != nil {
		return nil, errors.Wrap(err, "could not find Go projects")
	}

	var projects []module.Module
	for _, p := range found {
		projects = append(projects, module.Module{
			Name:         p.Name,
			Type:         pkg.Go,
			IsExecutable: p.Name == "main",
			BuildTarget:  p.ImportPath,
		})
	}
	return projects, nil
}

// Clean runs `go clean $PKG`.
func (a *Analyzer) Clean(m module.Module) error {
	return a.Go.Clean([]string{m.BuildTarget})
}

// Build runs `go build $PKG`.
func (a *Analyzer) Build(m module.Module) error {
	return a.Go.Build([]string{m.BuildTarget})
}

// IsBuilt runs `go list $PKG` and checks for errors.
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	log.Logger.Debugf("%#v", m)
	pkg, err := a.Go.ListOne(m.BuildTarget)
	if err != nil {
		return false, err
	}
	return pkg.Error == nil, nil
}

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("%#v", m)
	// Get Go project.
	project, err := a.GetProject(m.BuildTarget)
	if err != nil {
		return m, err
	}
	log.Logger.Debugf("Go project: %#v", project)

	// Read lockfiles to get revisions.
	var lockfile Resolver
	switch a.Options.Strategy {
	// Read revisions from a tool manifest at a specified location.
	case "manifest:dep":
		return m, errors.New("not yet implemented")
	case "manifest:gdm":
		return m, errors.New("not yet implemented")
	case "manifest:glide":
		return m, errors.New("not yet implemented")
	case "manifest:godep":
		return m, errors.New("not yet implemented")
	case "manifest:govendor":
		return m, errors.New("not yet implemented")
	case "manifest:vndr":
		return m, errors.New("not yet implemented")

	// Resolve revisions by traversing the local $GOPATH and calling the package's
	// VCS.
	case "gopath-vcs":
		return m, errors.New("not yet implemented")

	// Read revisions from an auto-detected tool manifest.
	default:
		lockfile, err = NewResolver(project.Tool, project.Manifest)
		if err != nil {
			return m, err
		}
	}

	log.Logger.Debugf("Lockfile: %#v", lockfile)

	// Use `go list` to get imports and deps of module.
	main, err := a.Go.ListOne(m.BuildTarget)
	if err != nil {
		return m, err
	}
	log.Logger.Debugf("Go main package: %#v", main)
	deps, err := a.Go.List(main.Deps)
	if err != nil {
		return m, err
	}

	// Construct map of unvendored import path to package.
	gopkgs := append(deps, main)
	gopkgMap := make(map[string]gocmd.Package)
	for _, p := range gopkgs {
		gopkgMap[Unvendor(p.ImportPath)] = p
	}
	// cgo imports don't have revisions.
	gopkgMap["C"] = gocmd.Package{
		Name:     "C",
		IsStdLib: true,
	}

	// Construct dependency graph.
	pkgs := make(map[pkg.ID]pkg.Package)
	_ = pkgs
	for _, gopkg := range gopkgs {
		log.Logger.Debugf("Getting revision for: %#v", gopkg)

		// Get revision.
		revision, err := GetRevision(project, lockfile, gopkg)
		if err != nil {
			return m, err
		}
		id := pkg.ID{
			Type:     pkg.Go,
			Name:     Unvendor(gopkg.ImportPath),
			Revision: revision,
			Location: "", // TODO: fill this field with something useful?
		}

		// Resolve imports.
		var imports []pkg.Import
		for _, i := range gopkg.Imports {
			name := Unvendor(i)
			revision, err := GetRevision(project, lockfile, gopkgMap[name])
			if err != nil {
				return m, err
			}
			imports = append(imports, pkg.Import{
				Target: name,
				Resolved: pkg.ID{
					Type:     pkg.Go,
					Name:     name,
					Revision: revision,
					Location: "",
				},
			})
		}

		pkgs[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
	}

	// Construct imports list.
	var imports []pkg.ID
	for _, i := range main.Imports {
		name := Unvendor(i)
		revision, err := GetRevision(project, lockfile, gopkgMap[name])
		if err != nil {
			return m, err
		}

		imports = append(imports, pkg.ID{
			Type:     pkg.Go,
			Name:     name,
			Revision: revision,
			Location: "",
		})
	}

	m.Deps = pkgs
	m.Imports = imports
	return m, nil
}

// GetRevision resolves a revision, returning errutil.ErrNoRevisionForPackage
// when no revision is found unless a revision is not required.
//
// Packages require a resolved revision unless:
//
//   1. The package is part of the standard library.
//   2. The package is internal.
//   3. The package is within the project.
//
func GetRevision(project Project, resolver Resolver, gopkg gocmd.Package) (string, error) {
	log.Logger.Debugf("GetRevision: %#v", gopkg)
	name := Unvendor(gopkg.ImportPath)
	revision, err := resolver.Resolve(name)
	if err == errutil.ErrNoRevisionForPackage {
		log.Logger.Debugf("Could not find revision for package %#v", name)
		if gopkg.IsStdLib || gopkg.IsInternal || strings.HasPrefix(gopkg.Dir, project.Dir) {
			log.Logger.Debugf("Skipping package: %#v", gopkg)
			return "", nil
		}
	}
	if err != nil {
		return "", err
	}
	return revision, nil
}
