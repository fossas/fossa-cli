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

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/analyzers/golang/resolver"
	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

// An Analyzer contains structs used in the analysis of Go packages. It
// implements analyzer.Analyzer.
type Analyzer struct {
	Go        gocmd.Go
	GoVersion string

	Options Options

	// These caches prevent redundant filesystem lookups and execs, and help a lot
	// when dealing with nested vendoring.
	resolverCache map[string]resolver.Resolver
	projectCache  map[string]Project
}

// Options set analyzer options for Go modules.
type Options struct {
	BuildOS               string `mapstructure:"os"`                      // Target build OS (for build tags).
	BuildArch             string `mapstructure:"arch"`                    // Target build architecture (for build tags).
	Strategy              string `mapstructure:"strategy"`                // See the Go analyzer documentation.
	LockfilePath          string `mapstructure:"lockfile"`                // For non-standard lockfile locations with strategies `manifest:*`.
	AllowUnresolved       bool   `mapstructure:"allow-unresolved"`        // Allow unresolved revisions.
	AllowUnresolvedPrefix string `mapstructure:"allow-unresolved-prefix"` // If set, allows unresolved revisions for packages whose import path's prefix matches.
	AllowNestedVendor     bool   `mapstructure:"allow-nested-vendor"`     // If true, allows vendor folders to be nested and attempts to resolve using parent lockfile lookup.
	AllowDeepVendor       bool   `mapstructure:"allow-deep-vendor"`       // If true, allows nested vendored dependencies to be resolved using ancestor lockfiles farther than their direct parent.
	SkipImportTracing     bool   `mapstructure:"skip-tracing"`            // If true, skips dependency tracing.
	SkipProject           bool   `mapstructure:"skip-project"`            // If true, skips project detection.
}

// New constructs an Analyzer.
func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debug("%#v", opts)

	// Parse and validate options.
	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}
	log.Logger.Debug("Decoded options: %#v", options)

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

		resolverCache: make(map[string]resolver.Resolver),
		projectCache:  make(map[string]Project),
	}, nil
}

// Discover runs `go list ./...`.
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	found, err := a.Go.List([]string{"./..."})
	if err != nil {
		return nil, errors.Wrap(err, "could not find Go projects")
	}

	var projects []module.Module
	for _, f := range found {
		log.Logger.Debugf("Found Go module: %#v", f)
		projects = append(projects, module.Module{
			Name:         Unvendor(f.ImportPath),
			Type:         pkg.Go,
			IsExecutable: f.Name == "main",
			BuildTarget:  f.ImportPath,
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
	log.Logger.Debug("%#v", m)
	pkg, err := a.Go.ListOne(m.BuildTarget)
	if err != nil {
		return false, err
	}
	return pkg.Error == nil, nil
}
