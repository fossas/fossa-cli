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

	"github.com/fossas/fossa-cli/buildtools/golang"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/project"
)

// An Analyzer contains structs used in the analysis of Go packages.
type Analyzer struct {
	Go        golang.Go
	GoVersion string
}

// New constructs an Analyzer given GoOptions.
func New(options project.GoOptions) (*Analyzer, error) {
	cmd, version, err := exec.Which("version", os.Getenv("FOSSA_GO_CMD"), "go")
	if err != nil {
		return nil, err
	}
	return &Analyzer{
		Go: golang.Go{
			Cmd:  cmd,
			OS:   options.BuildOS,
			Arch: options.BuildArch,
		},
		GoVersion: version,
	}, nil
}

// Discover runs `go list ./...`.
func (a *Analyzer) Discover(dir string) ([]project.Project, error) {
	a.Go.List([]string{"./..."})
	return nil, nil
}

// Clean runs `go clean $PKG`.
func (a *Analyzer) Clean(p project.Project) error {
	return a.Go.Clean([]string{p.BuildTarget})
}

// Build runs `go build $PKG`.
func (a *Analyzer) Build(p project.Project) error {
	return a.Go.Build([]string{p.BuildTarget})
}

// IsBuilt runs `go list $PKG` and checks for errors.
func (a *Analyzer) IsBuilt(p project.Project) (bool, error) {
	pkg, err := a.Go.ListOne(p.BuildTarget)
	if err != nil {
		return false, err
	}
	return pkg.Error == nil, nil
}

// Analyze builds a dependency graph using go list and then looks up revisions
// using tool-specific lockfiles.
func (a *Analyzer) Analyze(p project.Project) (project.Project, error) {
	options := p.Options.(project.GoOptions)
	switch options.Strategy {
	case "manifest:godep":
		return a.ResolveManifest(p)
	default:
		return p, nil
	}
}
