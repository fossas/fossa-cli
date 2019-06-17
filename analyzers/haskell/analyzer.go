package haskell

import (
	"github.com/mitchellh/mapstructure"

	"github.com/fossas/fossa-cli/buildtools/cabal"
	"github.com/fossas/fossa-cli/buildtools/stack"
	"github.com/fossas/fossa-cli/errors"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
)

type Analyzer struct {
	Module    module.Module
	AnalyzeFn func(module.Module) (graph.Deps, error)
}

func New(m module.Module) (*Analyzer, error) {
	var options Options
	err := mapstructure.Decode(m.Options, &options)

	if err != nil {
		return nil, err
	}

	if options.Strategy == CabalInstall {
		return &Analyzer{
			Module:    m,
			AnalyzeFn: cabal.GetDeps,
		}, nil
	} else if options.Strategy == Stack {
		return &Analyzer{
			Module:    m,
			AnalyzeFn: stack.GetDeps,
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
