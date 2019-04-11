
package haskell

import (
	"github.com/fossas/fossa-cli/analyzers/haskell/analyzer"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/mitchellh/mapstructure"
)

type Analyzer struct {
	Module    module.Module
	AnalyzeFn func(module.Module)(graph.Deps, error)
}

func New(m module.Module) (*Analyzer, error) {
	var options Options
	err := mapstructure.Decode(m.Options, &options)

	if err != nil {
		return nil, err
	}

	if options.Strategy == CabalInstall {
		return &Analyzer{
			Module:  m,
			AnalyzeFn: analyzer.AnalyzeCabal,
		}, nil
	} else if options.Strategy == Stack {
		return &Analyzer{
			Module:  m,
			AnalyzeFn: analyzer.AnalyzeStack,
		}, nil
	}

	return nil, errors.New("Unknown haskell analysis strategy")
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	return a.AnalyzeFn(a.Module)
}

func (Analyzer) Clean() error {
	return nil
}

func (Analyzer) Build() error {
	return nil
}

func (Analyzer) IsBuilt() (bool, error) {
	return true, nil
}
