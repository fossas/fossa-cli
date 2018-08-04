// Package module defines a FOSSA CLI module.
package module

import "github.com/fossas/fossa-cli/pkg"

// A Module is a single compilable unit of code (e.g. an entrypoint) to
// analyze. Modules consist of a root Package, and the transitive dependencies
// of that package.
type Module struct {
	Name         string   `yaml:"name"`             // Uniquely identifies the module.
	Type         pkg.Type `yaml:"type"`             // Type of the root package, used to select the module's analyzer.
	IsExecutable bool     `yaml:"-"`                // Used for filtering discovered projects.
	Ignore       bool     `yaml:"ignore,omitempty"` // Used for marking ignored (blacklisted) modules.

	BuildTarget string `yaml:"target,omitempty"` // Exact build target in the semantics of the module's analyzers.
	Dir         string `yaml:"cwd,omitempty"`    // CWD to analyze the module from.

	Options map[string]interface{} `yaml:"options,omitempty"` // Analyzer option struct of the module type.

	Imports []pkg.Import           `yaml:"-"` // Direct dependencies of the root package.
	Deps    map[pkg.ID]pkg.Package `yaml:"-"` // All transitive dependencies of the root package (including Imports).
}

// IsAnalyzed returns true if a module has already been analyzed, and false
// otherwise.
func (p *Module) IsAnalyzed() bool {
	return p.Imports != nil && p.Deps != nil
}
