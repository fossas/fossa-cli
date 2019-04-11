
package haskell

import (
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/mitchellh/mapstructure"
	"path/filepath"
)

type Options struct {
	// TODO: strategy as enum?
	Strategy string `mapstructure:"strategy"`
}

type Analyzer struct {
	Module  module.Module
	Options Options
}

func New(m module.Module) (*Analyzer, error) {
	var options Options
	err := mapstructure.Decode(m.Options, &options)

	if err != nil {
		return nil, err
	}

	return &Analyzer{
		Module:  m,
		Options: options,
	}, nil
}

func (a *Analyzer) Analyze() (graph.Deps, error) {
	if a.Options.Strategy == "cabal-install" {
		return a.AnalyzeCabal()
	} else if a.Options.Strategy == "stack" {
		return a.AnalyzeStack()
	} else {
		panic("Unknown haskell analysis strategy: " + a.Options.Strategy)
	}
	// TODO: dist-newstyle/cache/plan.json first
	// TODO: cabal new-build plan.json somehow? --dry-run?
	panic("implement me")
}

const CabalPlanRelPath = "dist-newstyle/cache/plan.json"

type CabalPlan struct {
	Packages []Package `mapstructure:"install-plan"`
}

type Package struct {
	Type    string   `mapstructure:"type"`
	Id      string   `mapstructure:"id"`
	Name    string   `mapstructure:"pkg-name"`
	Version string   `mapstructure:"pkg-version"`
	Blah    string   `mapstructure:"blah"`

	Depends []string `mapstructure:"depends"`
	Style   string   `mapstructure:"style"` // Only exists for packages with type `configured`
}

func (a *Analyzer) AnalyzeCabal() (graph.Deps, error) {
	cabalPlanPath := filepath.Join(a.Module.Dir, CabalPlanRelPath)

	if exists, _ := files.Exists(cabalPlanPath); exists {
		// Parse cabal new-build's build plan
		var rawPlan map[string]interface{}
		var plan    CabalPlan

		if err := files.ReadJSON(&rawPlan, cabalPlanPath); err != nil {
			return graph.Deps{}, err
		}
		if err := mapstructure.Decode(rawPlan, &plan); err != nil {
			return graph.Deps{}, err
		}

		var packages = make(map[string]Package)
		for _, p := range plan.Packages {
			packages[p.Id] = p
		}

		// Determine which of the projects in the plan are ours. Our projects
		// will have type "configured" and style "local"
		var ourProjects []Package

		for id, p := range packages {
			if p.Type == "configured" && p.Style == "local" {
				ourProjects = append(ourProjects, p)
				delete(packages, id)
			}
		}

		// Determine direct imports
		// TODO TODO : include components.*.depends ugh
		// TODO: do we need to include Target?
		var imports []pkg.Import

		for _, project := range ourProjects {
			if project.Depends != nil {
				for _, depId := range project.Depends {
					if dep, ok := packages[depId]; ok {
						delete(packages, depId)

						imports = append(imports, pkg.Import{
							// TODO: do we need to include Target?
							Resolved: pkg.ID{
								Type:     pkg.Haskell,
								Name:     dep.Name,
								Revision: dep.Version,
							},
						})
					}
				}
			}
		}

		// Add remaining packages as transitive deps

		var transitive = make(map[pkg.ID]pkg.Package)

		for _, project := range packages {
			pkgId := pkg.ID{
				Type: pkg.Haskell,
				Name: project.Name,
				Revision: project.Version,
			}
			transitive[pkgId] = pkg.Package{
				ID: pkgId,
			}
		}

		deps := graph.Deps{
			Direct: imports,
			Transitive: transitive,
		}

		return deps, nil
	}
	// TODO: cabal new-build plan.json somehow? --dry-run?
	panic("implementme")
}

func (a *Analyzer) AnalyzeStack() (graph.Deps, error) {
	// TODO: `stack ls dependencies --depth X`, split on space
	panic("implementme")
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
