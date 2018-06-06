package builderutil

import "github.com/fossas/fossa-cli/module"

type Imported struct {
	module.Locator
	From module.ImportPath
}

// Utilities for computing import paths
func ComputeImportPaths(deps []Imported) []module.Dependency {
	pathsSet := make(map[module.Locator]map[module.ImportPathString]bool)
	for _, dep := range deps {
		// Ignore "root" deps
		if dep.Locator.Fetcher == "root" {
			continue
		}
		_, ok := pathsSet[dep.Locator]
		if !ok {
			pathsSet[dep.Locator] = make(map[module.ImportPathString]bool)
		}
		pathsSet[dep.Locator][dep.From.String()] = true
	}

	var out []module.Dependency
	for locator, paths := range pathsSet {
		// This way an empty modulePaths marshals to JSON as `[]` instead of `null`
		modulePaths := make([]module.ImportPath, 0)
		for path := range paths {
			if path == "" {
				continue
			}
			modulePaths = append(modulePaths, module.ReadImportPath(path))
		}
		out = append(out, module.Dependency{
			Locator: locator,
			Via:     modulePaths,
		})
	}

	return out
}
