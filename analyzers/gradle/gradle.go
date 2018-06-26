package gradle

import (
	"os"

	"github.com/fossas/fossa-cli/exec"
	"github.com/fossas/fossa-cli/log"
	"github.com/mitchellh/mapstructure"
)

type Analyzer struct {
	GradleCmd     string
	GradleVersion string

	Options Options
}

type Options struct {
	AllowOnline   bool
	GradleProject string
	GradleTask    string
	GradleConfig  string
}

func New(opts map[string]interface{}) (*Analyzer, error) {
	log.Logger.Debugf("%#v", opts)

	gradleCmd, gradleVersion, err := exec.Which("-v", os.Getenv("FOSSA_GRADLE_CMD"), "gradlew", "gradle")
	if err != nil {
		log.Logger.Warningf("Could not find Gradle: %s", err.Error())
	}

	var options Options
	err := mapstructure.Decode(opts, &options)
	if err != nil {
		return nil, err
	}

	analyzer := Analyzer{
		GradleCmd:     gradleCmd,
		GradleVersion: gradleVersion,

		Options: options,
	}

	log.Logger.Debugf("Initialized Gradle analyzer: %#v", analyzer)
	return &analyzer, nil
}

// Discover searches for `*.gradle` files and runs `gradle tasks`
func (a *Analyzer) Discover(dir string) ([]module.Module, error) {
	log.Logger.Debugf("%#v", dir)
	var modules []module.Module
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
			return err
		}

		// Skip **/node_modules and **/bower_components
		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
			log.Logger.Debugf("Skipping directory: %s", info.Name())
			return filepath.SkipDir
		}

		if !info.IsDir() && info.Name() == "package.json" {
			name := filepath.Base(filepath.Dir(path))
			// Parse from project name from `package.json` if possible
			if manifest, err := npm.FromManifest(path); err == nil && manifest.Name != "" {
				name = manifest.Name
			}

			log.Logger.Debugf("Found NodeJS project: %s (%s)", path, name)
			path, err = filepath.Rel(dir, path)
			if err != nil {
				log.Logger.Panicf("Could not construct NodeJS project path", err.Error())
			}
			modules = append(modules, module.Module{
				Name:        name,
				Type:        pkg.NodeJS,
				BuildTarget: path,
				Dir:         filepath.Dir(path),
			})
		}
		return nil
	})

	if err != nil {
		return nil, errors.Wrap(err, "could not find NodeJS projects")
	}

	return modules, nil
}

func (a *Analyzer) Clean(m module.Module) error {
	return files.Rm(m.Dir, "node_modules")
}

// Build runs `yarn install --production --frozen-lockfile` if there exists a
// `yarn.lock` and `yarn` is available. Otherwise, it runs
// `npm install --production`.
func (a *Analyzer) Build(m module.Module) error {
	log.Logger.Debugf("Running Node.js build: %#v", m)

	// Prefer Yarn where possible
	if ok, err := files.Exists(m.Dir, "yarn.lock"); err == nil && ok && a.YarnCmd != "" {
		_, _, err := exec.Run(exec.Cmd{
			Name: a.YarnCmd,
			Argv: []string{"install", "--production", "--frozen-lockfile"},
			Dir:  m.Dir,
		})
		if err != nil {
			return errors.Wrap(err, "could not run `yarn` build")
		}
	} else if a.NPMCmd != "" {
		_, _, err := exec.Run(exec.Cmd{
			Name: a.NPMCmd,
			Argv: []string{"install", "--production"},
			Dir:  m.Dir,
		})
		if err != nil {
			return errors.Wrap(err, "could not run `npm` build")
		}
	} else {
		return errors.New("no Node.JS build tools detected")
	}

	log.Logger.Debug("Done running Node.js build.")
	return nil
}

// IsBuilt returns true if a project has a manifest and either has no
// dependencies or has a `node_modules` folder.
//
// Note that there could be very strange builds where this will produce false
// negatives (e.g. `node_modules` exists in a parent folder). There can also
// exist builds where this will produce false positives (e.g. `node_modules`
// folder does not include the correct dependencies). We also don't take
// $NODE_PATH into account during resolution.
//
// TODO: with significantly more effort, we can eliminate both of these
// situations.
func (a *Analyzer) IsBuilt(m module.Module) (bool, error) {
	log.Logger.Debugf("Checking Node.js build: %#v", m)

	manifest, err := npm.FromManifest(filepath.Dir(m.BuildTarget))
	if err != nil {
		return false, errors.Wrap(err, "could not parse package manifest to check build")
	}

	if len(manifest.Dependencies) > 0 {
		log.Logger.Debugf("Done checking Node.js build: project has no dependencies")
		return true, nil
	}

	hasNodeModules, err := files.ExistsFolder(m.Dir, "node_modules")
	if err != nil {
		return false, err
	}

	log.Logger.Debugf("Done checking Node.js build: %#v", hasNodeModules)
	return hasNodeModules, nil
}

func (a *Analyzer) Analyze(m module.Module) (module.Module, error) {
	log.Logger.Debugf("Running Nodejs analysis: %#v", m)

	// Get packages.
	n := npm.NPM{
		Cmd:      a.NPMCmd,
		AllowErr: a.Options.AllowNPMErr,
	}
	pkgs, err := n.List(filepath.Dir(m.BuildTarget))
	if err != nil {
		log.Logger.Warningf("NPM had non-zero exit code: %s", err.Error())
	}

	// Set direct dependencies.
	var imports []pkg.Import
	for name, dep := range pkgs.Dependencies {
		imports = append(imports, pkg.Import{
			Target: dep.From,
			Resolved: pkg.ID{
				Type:     pkg.NodeJS,
				Name:     name,
				Revision: dep.Version,
				Location: dep.Resolved,
			},
		})
	}

	// Set transitive dependencies.
	deps := make(map[pkg.ID]pkg.Package)
	recurseDeps(deps, pkgs)

	m.Imports = imports
	m.Deps = deps
	log.Logger.Debugf("Done running Nodejs analysis: %#v", deps)
	return m, nil
}

func recurseDeps(pkgMap map[pkg.ID]pkg.Package, p npm.Output) {
	for name, dep := range p.Dependencies {
		// Construct ID.
		id := pkg.ID{
			Type:     pkg.NodeJS,
			Name:     name,
			Revision: dep.Version,
			Location: dep.Resolved,
		}
		// Don't process duplicates.
		_, ok := pkgMap[id]
		if ok {
			continue
		}
		// Get direct imports.
		var imports []pkg.Import
		for name, i := range p.Dependencies {
			imports = append(imports, pkg.Import{
				Target: i.From,
				Resolved: pkg.ID{
					Type:     pkg.NodeJS,
					Name:     name,
					Revision: i.Version,
					Location: i.Resolved,
				},
			})
		}
		// Update map.
		pkgMap[id] = pkg.Package{
			ID:      id,
			Imports: imports,
		}
		// Recurse in imports.
		recurseDeps(pkgMap, dep)
	}
}
