package fossa

import (
	"github.com/fossas/fossa-cli/graph"
	"github.com/fossas/fossa-cli/module"
	"github.com/fossas/fossa-cli/pkg"
)

type ApiModule struct {
	Filepath module.Filepath `json:"filepath"`
	Analyses []ApiAnalysis   `json:"analyses"`
}

type ApiAnalysis struct {
	AnalyzerName string           `json:"analyzer"`
	Graphs       []ApiTaggedGraph `json:"strategies"`
}

type ApiTaggedGraph struct {
	Strategy   string   `json:"strategy"`
	TargetFile string   `json:"targetFile"`
	Deps       []ApiDep `json:"depgraph"`
}

type ApiDep struct {
	Type     string `json:"type"`
	Name     string `json:"name"`
	Revision string `json:"revision"`

	Location     string   `json:"location,omitempty"`
	Dependencies []ApiDep `json:"dependencies,omitempty"`
}

// TODO: remove these
//Locator string   `json:"locator"`
//Imports []string `json:"imports,omitempty"`

func ApiFormatModules(scanned map[module.Filepath][]module.Analysis) []ApiModule {
	var result []ApiModule

	for path, analyses := range scanned {
		result = append(result, ApiModule{
			Filepath: path,
			Analyses: ApiFormatAnalyses(analyses),
		})
	}

	return result
}

func ApiFormatAnalyses(analyses []module.Analysis) []ApiAnalysis {
	var result []ApiAnalysis

	for _, analysis := range analyses {
		result = append(result, ApiAnalysis{
			AnalyzerName: analysis.AnalyzerName,
			Graphs:       ApiFormatGraphs(analysis.Graphs),
		})
	}

	return result
}

func ApiFormatGraphs(graphs []module.TaggedGraph) []ApiTaggedGraph {
	var result []ApiTaggedGraph

	for _, graph := range graphs {
		result = append(result, ApiTaggedGraph{
			Strategy:   graph.Strategy,
			TargetFile: graph.File,
			Deps:       ApiFormatDeps(graph.Graph),
		})
	}

	return result
}

func ApiFormatDeps(deps graph.Deps) []ApiDep {
	var result []ApiDep

	for _, direct := range deps.Direct {
		result = append(result, ApiFormatPackage(deps.Transitive, deps.Transitive[direct.Resolved]))
	}

	return result
}

func ApiFormatPackage(allDeps map[pkg.ID]pkg.Package, pkg pkg.Package) ApiDep {
	var deeper []ApiDep

	for _, pkgImport := range pkg.Imports {
		// TODO: do we ever deal with unresolved imports here?
		dep, ok := allDeps[pkgImport.Resolved]
		if !ok {
			continue
		}
		deeper = append(deeper, ApiFormatPackage(allDeps, dep))
	}

	return ApiDep{
		Type:         pkg.ID.Type.String(),
		Name:         pkg.ID.Name,
		Revision:     pkg.ID.Revision,
		Location:     pkg.ID.Location,
		Dependencies: deeper,
	}
}
