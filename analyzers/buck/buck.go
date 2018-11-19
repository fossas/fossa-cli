package buck

import (
	"github.com/fossas/fossa-cli/buildtools/buckaudit"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
)

// Analyzer defines a Buck analyzer
type Analyzer struct {
	Module module.Module
}

// Options??

// New constructs a new Buck analyzer from a module
func New(module module.Module) (*Analyzer, error) {

	analyzer := Analyzer{
		Module: module,
	}
	return &analyzer, nil
}

// Clean is used in the Analyzer interface
func (a *Analyzer) Clean() error {
	return nil
}

// Build is used in the Analyzer interface
func (a *Analyzer) Build() error {
	return nil
}

// IsBuilt is used in the Analyzer interface
func (a *Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

// Analyze analyzers a buck package and its dependencies
func (a *Analyzer) Analyze() (graph.Deps, error) {
	return buckaudit.Deps()
}
