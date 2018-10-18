package raw

import (
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type Analyzer struct {
	Module module.Module
}

func New(m module.Module) (*Analyzer, error) {
	return &Analyzer{
		Module: m,
	}, nil
}

func (a Analyzer) Clean() error {
	return nil
}

func (a Analyzer) Build() error {
	return nil
}

func (a Analyzer) IsBuilt() (bool, error) {
	return true, nil
}

func (a Analyzer) Analyze() (graph.Deps, error) {
	id := pkg.ID{
		Type: pkg.Raw,
		Name: a.Module.Name,
		// we cannot determine the revision without performing an upload. Let the caller overwrite this value if actually uploading.
		// If the user requests output only, we don't need to compute the actual revision
		Revision: "stubbedRevision",
	}

	return graph.Deps{
		Direct:     []pkg.Import{pkg.Import{Resolved: id}},
		Transitive: make(map[pkg.ID]pkg.Package),
	}, nil
}
