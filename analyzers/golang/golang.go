//
//
// Strategies:
// - default
// - use specific tool, read specific manifest at weird location (anki)
//
// options:
// - allow unresolved
// - allow unresolved prefix (hashi govendor)
package golang

import (
	"os"

	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/gocmd"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/pkg/errors"
)

// An Analyzer contains structs used in the analysis of Go packages.
type Analyzer struct {
	Go        gocmd.Go
	GoVersion string

	Options Options
}

// Options set analyzer options for Go modules.
type Options struct {
	BuildOS               string   `mapstructure:"os"`                      // The target build OS (for build tags).
	BuildArch             string   `mapstructure:"arch"`                    // The target build architecture (for build tags).
	Strategy              string   `mapstructure:"strategy"`                // See the Go analyzer documentation.
	Strategies            []string `mapstructure:"strategies"`              // Fallback strategies to try in succession.
	LockfilePath          string   `mapstructure:"lockfile"`                // For non-standard lockfile locations.
	AllowUnresolved       bool     `mapstructure:"allow-unresolved"`        // Allow unresolved revisions.
	AllowUnresolvedPrefix string   `mapstructure:"allow-unresolved-prefix"` // If set, restricts unresolved revisions to only those that match the prefix.
	SkipImportTracing     bool     `mapstructure:"skip-tracing"`            // If true, skips dependency tracing.
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
func (a *Analyzer) Clean(p module.Module) error {
	return a.Go.Clean([]string{p.BuildTarget})
}

// Build runs `go build $PKG`.
func (a *Analyzer) Build(p module.Module) error {
	return a.Go.Build([]string{p.BuildTarget})
}

// IsBuilt runs `go list $PKG` and checks for errors.
func (a *Analyzer) IsBuilt(p module.Module) (bool, error) {
	pkg, err := a.Go.ListOne(p.BuildTarget)
	if err != nil {
		return false, err
	}
	return pkg.Error == nil, nil
}

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze(p module.Module) (module.Module, error) {
	switch a.Options.Strategy {
	case "manifest:godep":
		return a.ResolveManifest(p)
	default:
		return p, nil
	}
}
