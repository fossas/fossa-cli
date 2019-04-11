package analyzer

import (
	"errors"
	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/mitchellh/mapstructure"
	"path/filepath"
)

const CabalPlanRelPath = "dist-newstyle/cache/plan.json"

type CabalPlan struct {
	Packages []Package `mapstructure:"install-plan"`
}

type Package struct {
	Type    string   `mapstructure:"type"`
	Id      string   `mapstructure:"id"`
	Name    string   `mapstructure:"pkg-name"`
	Version string   `mapstructure:"pkg-version"`

	Components map[string]Component `mapstructure:"components"` // Not always present
	Depends    []string             `mapstructure:"depends"`    // Dependencies can be defined here or in Components.*.Depends
	Style      string               `mapstructure:"style"`      // Only exists for packages with type `configured`
}

type Component struct {
	Depends []string `mapstructure:"depends"`
}

func AnalyzeCabal(m module.Module) (graph.Deps, error) {
	cabalPlanPath := filepath.Join(m.Dir, CabalPlanRelPath)

	// If plan.json doesn't exist, generate it
	if exists, _ := files.Exists(cabalPlanPath); !exists {
		_, _, err := exec.Run(exec.Cmd{
			Name: "cabal",
			Argv: []string{"new-build", "--dry-run"},
		})

		if err != nil {
			return graph.Deps{}, err
		}
	}

	if exists, _ := files.Exists(cabalPlanPath); !exists {
		// TODO: fallback to another strategy?
		return graph.Deps{}, errors.New("couldn't find or generate cabal solver plan")
	}

	// Parse cabal new-build's build plan
	var rawPlan map[string]interface{}
	var plan    CabalPlan

	if err := files.ReadJSON(&rawPlan, cabalPlanPath); err != nil {
		return graph.Deps{}, err
	}
	if err := mapstructure.Decode(rawPlan, &plan); err != nil {
		return graph.Deps{}, err
	}

	// Elements in this map are packages that haven't been scanned for
	// dependencies yet
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
	var imports []pkg.Import

	for _, project := range ourProjects {
		// todo: duplicate code
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
		for _, component := range project.Components {
			// todo: duplicate code
			for _, depId := range component.Depends {
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