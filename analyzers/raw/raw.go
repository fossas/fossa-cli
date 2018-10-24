package raw

import (
	"github.com/apex/log"

	"github.com/fossas/fossa-cli/api/fossa"
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
	// The locator for the tarball is determined serverside, so we must upload the tarball before we can build the depGraph.
	locator, err := fossa.UploadTarball(a.Module.BuildTarget)
	if err != nil {
		log.Warnf("Could not upload raw module: %s", err.Error())
	}
	id := pkg.ID{
		Type:     pkg.Raw,
		Name:     locator.Project,
		Revision: locator.Revision,
	}

	return graph.Deps{
		Direct:     []pkg.Import{pkg.Import{Resolved: id}},
		Transitive: make(map[pkg.ID]pkg.Package),
	}, nil
}
