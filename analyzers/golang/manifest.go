package golang

import (
	"path/filepath"

	"github.com/fossas/fossa-cli/buildtools/godep"
	"github.com/fossas/fossa-cli/pkg"
	"github.com/fossas/fossa-cli/project"
)

func (a *Analyzer) ResolveManifest(p project.Project) (project.Project, error) {
	options := p.Options.(project.GoOptions)
	// Read the lockfile.
	lockfilePath := options.LockfilePath
	if lockfilePath == "" {
		lockfilePath = filepath.Join(p.Dir, "Godeps", "Godeps.json")
	}
	godepPkgs, err := godep.ReadFile(options.LockfilePath)
	if err != nil {
		return p, err
	}
	revisions := make(map[string]godep.Package)
	for _, godepPkg := range godepPkgs {
		revisions[godepPkg.ImportPath] = godepPkg
	}

	// Trace dependencies.
	// Get direct dependencies.
	goPkg, err := a.Go.ListOne(p.BuildTarget)
	if err != nil {
		return p, err
	}
	for _, goImport := range goPkg.Imports {
		revision, ok := revisions[goImport]
		if !ok {
			// we're missing a lock
		}
		p.Imports = append(p.Imports, pkg.ID{
			Type:     pkg.Go,
			Name:     goImport,
			Revision: revision.Rev,
			Location: goImport,
		})
	}
	// Get transitive dependencies.
	var pkgnames []string
	for _, godepPkg := range godepPkgs {
		pkgnames = append(pkgnames, godepPkg.ImportPath)
	}
	goPkgs, err := a.Go.List(pkgnames)
	if err != nil {
		return p, err
	}
	p.Deps = make(map[pkg.ID]pkg.Package)
	for _, goPkg := range goPkgs {
		revision, ok := revisions[goPkg.Name]
		if !ok {
			// we're missing a lock
		}
		id := pkg.ID{
			Type:     pkg.Go,
			Name:     goPkg.Name,
			Revision: revision.Rev,
			Location: goPkg.Name,
		}
		var imports []pkg.Import
		for _, imported := range goPkg.Imports {
			importedRevision, ok := revisions[imported]
			if !ok {
				// we're missing a lock
			}
			imports = append(imports, pkg.Import{
				Target: "",
				Resolved: &pkg.ID{
					Type:     pkg.Go,
					Name:     imported,
					Revision: importedRevision.Rev,
					Location: imported,
				},
			})
		}
		p.Deps[id] = pkg.Package{
			ID:       id,
			Imports:  imports,
			Strategy: "manifest:godep",
		}
	}
	return p, nil
}
