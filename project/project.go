// Package project defines a FOSSA CLI project.
package project

import "github.com/fossas/fossa-cli/pkg"

// A Project is a single compilable unit of code (e.g. an entrypoint) to
// analyze. Projects consist of a root Package, and the transitive dependencies
// of that package.
type Project struct {
	Name         string   `yaml:"name"` // Uniquely identifies the project.
	Type         pkg.Type `yaml:"type"` // The type of the root package, used to select the project's analyzer.
	IsExecutable bool     `yaml:"-"`    // Used for filtering discovered projects.

	BuildTarget string `yaml:"target,omitempty"` // The exact build target in the semantics of the project's analyzers.
	Dir         string `yaml:"cwd,omitempty"`    // The CWD to analyze the project from.

	Options interface{} `yaml:"options,omitempty"` // The analyzer option struct of the project type.

	Imports []pkg.ID               `yaml:"-"` // Direct dependencies of the root package.
	Deps    map[pkg.ID]pkg.Package `yaml:"-"` // All transitive dependencies of the root package (including Imports).
}

// IsAnalyzed returns true if a project has already been analyzed, and false
// otherwise.
func (p *Project) IsAnalyzed() bool {
	return p.Imports != nil && p.Deps != nil
}

// GoOptions sets analyzer options for Go projects.
type GoOptions struct {
	BuildOS         string // The target build OS (for build tags).
	BuildArch       string // The target build architecture (for build tags).
	Strategy        string // See the Go analyzer documentation.
	LockfilePath    string // For non-standard lockfile locations.
	AllowUnresolved bool   // Allow unresolved revisions.
	// AllowUnresolvedPrefix string // If set, restricts unresolved revisions to only those that match the prefix.
	// SkipTrace             bool   // If true, skips dependency tracing.
}
