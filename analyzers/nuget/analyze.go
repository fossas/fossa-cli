package nuget

import (
	"path/filepath"
	"regexp"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/buildtools/paket"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
)

func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.WithField("module", a.Module).Debug("analyzing module")
	dir := Dir(a.Module)
	if dir == "." {
		dir = ""
	}
	// Strategies
	// 1. Paket
	// 2. check for projects.assets.json
	// 3. analyze the build target
	// 4. packages.config & project.json

	// Questions
	// This is heavily reliant on the discovery stage to find the correct file, difficult to handle for package reference which is a significant amount of projects.
	// Currently I would like to discover the correct file between package reference, to nuspec, to packages.config.
	// This doesn't allow for fallbacks if the file cannot be read

	switch a.Options.Strategy {
	case "paket":
		return paket.DependencyGraph(a.Module.BuildTarget)
	case "packagereference":
		return dotnet.PackageReferenceGraph(a.Module.BuildTarget)
	case "nuspec":
		return dotnet.NuspecGraph(a.Module.BuildTarget)
	case "packageconfig":
		return dotnet.ProjectGraph(filepath.Join(dir, "packages.config"))
	case "projectjson":
		return dotnet.ProjectGraph(filepath.Join(dir, "project.json"))
	default:
		if exists, err := files.Exists(filepath.Join(dir, "paket.lock")); exists && err == nil {
			return paket.DependencyGraph(filepath.Join(dir, "paket.lock"))
		}

		if exists, err := files.Exists(filepath.Join(dir, "obj", "project.assets.json")); exists && err == nil {
			depGraph, err := dotnet.ResolveStrategy(a.Module.BuildTarget, a.Module.Dir)
			if err == nil && len(depGraph.Transitive) > 0 {
				return depGraph, nil
			}
			log.Warnf("There was a problem reading dependencies from project.assets.json: %s, %+v", err.Error(), depGraph)
		}

		var xmlProj = regexp.MustCompile(`.*\.(cs|x|vb|db|fs)proj$`)
		if xmlProj.MatchString(a.Module.BuildTarget) {
			exists, err := files.Exists(a.Module.BuildTarget)
			if exists && err == nil {
				graph, err := dotnet.PackageReferenceGraph(a.Module.BuildTarget)
				if err == nil && len(graph.Transitive) > 0 {
					return graph, nil
				}
				log.Warnf("There was a problem analyzing the package reference file `%s`. Error: %s. Graph: %+v", a.Module.BuildTarget, err, graph)
			} else {
				log.Warnf("Target `%s` is unable to be read. File exists: %b. Error: %s", a.Module.BuildTarget, exists, err)
			}
		}

		pattern := filepath.Join(a.Module.Dir, "*nuspec")
		fileMatches, err := filepath.Glob(pattern)
		if err == nil && len(fileMatches) > 0 {
			file := fileMatches[0]
			graph, err := dotnet.NuspecGraph(file)
			if err == nil && len(graph.Transitive) > 0 {
				return graph, nil
			}
			log.Warnf("There was a problem analyzing the nuspec file `%s`. Error: %s. Graph: %+v", file, err, graph)
		}

		if exists, err := files.Exists(filepath.Join(dir, "packages.config")); exists && err == nil {
			graph, err := dotnet.PackageConfigGraph(filepath.Join(dir, "packages.config"))
			if err == nil && len(graph.Transitive) > 0 {
				return graph, err
			}
			log.Warnf("There was a problem analyzing packages.config. Error: %s. Graph: %+v", err, graph)
		}

		if exists, err := files.Exists(filepath.Join(dir, "project.json")); exists && err == nil {
			graph, err := dotnet.ProjectGraph(filepath.Join(dir, "project.json"))
			if err == nil && len(graph.Transitive) > 0 {
				return graph, err
			}
			log.Warnf("There was a problem analyzing project.json. Error: %s. Graph: %+v", err, graph)
		}
	}

	return graph.Deps{}, nil
}
