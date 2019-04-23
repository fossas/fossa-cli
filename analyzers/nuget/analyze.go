package nuget

import (
	"fmt"
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

		// Try to use the resolving method js
		if exists, err := files.Exists(filepath.Join(dir, "obj", "project.assets.json")); exists && err == nil {
			graph, err := dotnet.ResolveStrategy(a.Module.BuildTarget, a.Module.Dir)
			if err == nil {
				return graph, nil
			}
			log.Warnf("Error trying to get dependencies from project.assets.json: %s", err.Error())
		}
		log.Warn("project.assets.json has not been found")

		// Use a packageReference function xml
		var xmlProj = regexp.MustCompile(`.*\.(cs|x|vb|db|fs)proj$`)
		if xmlProj.MatchString(a.Module.BuildTarget) {
			if exists, err := files.Exists(a.Module.BuildTarget); !exists || err != nil {
				fmt.Println("file doesn't exist or error", err, exists)
			}
			graph, err := dotnet.PackageReferenceGraph(a.Module.BuildTarget)
			if err == nil && len(graph.Transitive) > 0 {
				return graph, nil
			}
			log.Warnf("Error trying to read package reference file `%s`: %+v", a.Module.BuildTarget, err)
		}

		pattern := filepath.Join(a.Module.Dir, "*nuspec")
		if fileMatches, err := filepath.Glob(pattern); err == nil && len(fileMatches) > 0 {
			file := fileMatches[0]
			if exists, err := files.Exists(file); !exists || err != nil {
				fmt.Println("file doesn't exist or error", err, exists)
			}
			graph, err := dotnet.NuspecGraph(file)
			if err == nil {
				return graph, err
			}
			log.Warnf("Error trying to read package reference file `%s`: %+v", a.Module.BuildTarget, err.Error())
		}

		if exists, err := files.Exists(filepath.Join(dir, "packages.config")); exists && err == nil {
			graph, err := dotnet.PackageConfigGraph(filepath.Join(dir, "packages.config"))
			if err == nil {
				return graph, err
			}
		}

		if exists, err := files.Exists(filepath.Join(dir, "project.json")); exists && err == nil {
			graph, err := dotnet.ProjectGraph(filepath.Join(dir, "project.json"))
			if err == nil {
				return graph, err
			}
		}
	}

	return graph.Deps{}, nil
}
