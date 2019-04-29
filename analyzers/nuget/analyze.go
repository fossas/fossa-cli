package nuget

import (
	"path/filepath"

	"github.com/apex/log"
	"github.com/fossas/fossa-cli/buildtools/dotnet"
	"github.com/fossas/fossa-cli/buildtools/paket"
	"github.com/fossas/fossa-cli/files"
	"github.com/fossas/fossa-cli/graph"
)

// Analyze determines the best way to analyze a .NET project by first looking for
// a pre set strategy and then falling back through a number of methods.
func (a *Analyzer) Analyze() (graph.Deps, error) {
	log.WithField("module", a.Module).Debug("analyzing module")
	dir := Dir(a.Module)

	// 1. Check for a set strategy.
	switch a.Options.Strategy {
	case "paket":
		return paket.DependencyGraph(a.Module.BuildTarget)
	case "package-reference":
		return dotnet.PackageReferenceGraph(a.Module.BuildTarget)
	case "nuspec":
		return dotnet.NuspecGraph(a.Module.BuildTarget)
	case "package-config":
		return dotnet.ProjectGraph(filepath.Join(dir, "packages.config"))
	case "project-json":
		return dotnet.ProjectGraph(filepath.Join(dir, "project.json"))
	}

	// 2. Check for Paket.
	// https://fsprojects.github.io/Paket/
	if exists, err := files.Exists(filepath.Join(dir, "paket.lock")); exists && err == nil {
		return paket.DependencyGraph(filepath.Join(dir, "paket.lock"))
	}

	// 3. Check for `project.assets.json` which is the nuget lockfile.
	// https://docs.microsoft.com/en-us/nuget/consume-packages/dependency-resolution
	if exists, err := files.Exists(filepath.Join(dir, "obj", "project.assets.json")); exists && err == nil {
		depGraph, err := dotnet.ResolveStrategy(a.Module.BuildTarget, a.Module.Dir)
		if returnGraph(a.Module.BuildTarget, err, len(depGraph.Transitive)) {
			return depGraph, nil
		}
	}

	// 4. Check for a Package Reference file used by NuGet 4.0+.
	// https://docs.microsoft.com/en-us/nuget/consume-packages/package-references-in-project-files
	if dotnet.IsPackageReferenceFile(a.Module.BuildTarget) {
		exists, err := files.Exists(a.Module.BuildTarget)
		if !exists || err != nil {
			log.Warnf("`%s` cannot be read. File exists: %b. Error: %s", a.Module.BuildTarget, exists, err)
		} else {
			depGraph, err := dotnet.PackageReferenceGraph(a.Module.BuildTarget)
			if returnGraph(a.Module.BuildTarget, err, len(depGraph.Transitive)) {
				return depGraph, nil
			}
		}
	}

	// 5. Check if a nuspec file exists and if it contains dependency information.
	// https://docs.microsoft.com/en-us/nuget/reference/nuspec
	pattern := filepath.Join(a.Module.Dir, "*nuspec")
	fileMatches, err := filepath.Glob(pattern)
	if err == nil && len(fileMatches) > 0 {
		file := fileMatches[0]
		depGraph, err := dotnet.NuspecGraph(file)
		if returnGraph(file, err, len(depGraph.Transitive)) {
			return depGraph, nil
		}
	}

	// 6. Check for a `packages.config` file.
	// https://docs.microsoft.com/en-us/nuget/reference/packages-config
	if exists, err := files.Exists(filepath.Join(dir, "packages.config")); exists && err == nil {
		file := filepath.Join(dir, "packages.config")
		depGraph, err := dotnet.PackageConfigGraph(file)
		if returnGraph(file, err, len(depGraph.Transitive)) {
			return depGraph, nil
		}
	}

	// 7. Check for a `project.json` file which was used until NuGet 4.0.
	// https://docs.microsoft.com/en-us/nuget/archive/project-json
	if exists, err := files.Exists(filepath.Join(dir, "project.json")); exists && err == nil {
		file := filepath.Join(dir, "project.json")
		depGraph, err := dotnet.ProjectGraph(file)
		if returnGraph(file, err, len(depGraph.Transitive)) {
			return depGraph, nil
		}
	}

	return graph.Deps{}, nil
}

func returnGraph(file string, err error, dependencyCount int) bool {
	if err != nil {
		log.Warnf("Error reading dependencies from `%s`: %s", file, err)
		return false
	}

	if dependencyCount == 0 {
		log.Warnf("No dependencies were found in `%s`", file)
		return false
	}
	return true
}
