// Package nodejs provides analyzers for Node.js projects.
//
// A Node.js project is defined as any folder with a `package.json`. A project
// may or may not have dependencies.
//
// A `BuildTarget` for Node.js is defined as the relative path to the directory
// containing the `package.json`, and the `Dir` is defined as the CWD for
// running tools.
//
// `npm` and `yarn` are explicitly supported as first-class tools. Where
// possible, these tools are queried before falling back to other strategies.
//
// All Node.js projects are implicitly supported via `node_modules` parsing.
package nodejs

// import (
// 	"os"
// 	"path/filepath"

// 	"github.com/fossas/fossa-cli/buildtools/npm"
// 	"github.com/fossas/fossa-cli/files"

// 	"github.com/pkg/errors"

// 	"github.com/fossas/fossa-cli/analyzers/analyzerutil"
// 	"github.com/fossas/fossa-cli/exec"
// 	"github.com/fossas/fossa-cli/log"
// 	"github.com/fossas/fossa-cli/project"
// 	"github.com/fossas/fossa-cli/pkg"
// )

// var (
// 	ErrNoNodeJS = errors.New("")
// 	ErrNoNPM    = errors.New("")
// 	ErrNoYarn   = errors.New("")
// )

// var Key = "nodejs"

// type NodeJSAnalyzer struct {
// 	nodeCmd string
// 	nodeOK  bool

// 	npmCmd string
// 	npmOK  bool

// 	yarnCmd string
// 	yarnOK  bool

// 	// options NodeJSOptions
// }

// // TODO: implement strategies.
// // // NodeJSOptions contains options for the `NodeJSAnalyzer`.
// // //
// // // The analyzer can use many different strategies. These are:
// // //
// // //   - `yarn`: Run and parse `yarn ls --json`.
// // //   - `npm`: Run and parse `npm ls --json`.
// // //   - `yarn.lock`: Parse `./yarn.lock`.
// // //   - `package-lock.json`: Parse `./package-lock.json`.
// // //   - `node_modules`: Parse `./package.json` and recursively look up dependencies with `node_modules` resolution.
// // //   - `node_modules_local`: Parse manifests in `./node_modules``.
// // //   - `package.json`: Parse `./package.json`.
// // //
// // // If no strategies are specified, the analyzer will try each of these
// // // strategies in descending order.
// // type NodeJSOptions struct {
// // 	// Strategies to try sequentially.
// // 	Strategies []string
// //
// // 	// One of "yarn", "npm", "node_modules", "yarn.lock", "package-lock.json", "package.json"
// // 	Strategy string
// //
// // 	BuildStrategies []string
// // 	BuildStrategy string
// // 	CleanBeforeBuild bool
// // }

// // New configures Node, NPM, and Yarn commands.
// func New() *NodeJSAnalyzer {
// 	log.Logger.Debug("Initializing Node.js analyzer...")

// 	nodeCmd, _, nodeErr := exec.Which("-v", os.Getenv("NODE_BINARY"), "node", "nodejs")
// 	npmCmd, _, npmErr := exec.Which("-v", os.Getenv("NPM_BINARY"), "npm")
// 	yarnCmd, _, yarnErr := exec.Which("-v", os.Getenv("YARN_BINARY"), "yarn")

// 	analyzer := NodeJSAnalyzer{
// 		nodeCmd: nodeCmd,
// 		nodeOK:  nodeErr == nil,

// 		npmCmd: npmCmd,
// 		npmOK:  npmErr == nil,

// 		yarnCmd: yarnCmd,
// 		yarnOK:  yarnErr == nil,
// 	}

// 	log.Logger.Debugf("Initialized Node.js analyzer: %#v", analyzer)
// 	return &analyzer
// }

// // Discover searches for `package.json`s not within a `node_modules` or
// // `bower_components`.
// func (n *NodeJSAnalyzer) Discover(dir string) ([]project.Project, error) {
// 	var programs []project.Project
// 	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
// 		if err != nil {
// 			log.Logger.Debugf("Failed to access path %s: %s\n", path, err.Error())
// 			return err
// 		}

// 		// Skip **/node_modules and **/bower_components
// 		if info.IsDir() && (info.Name() == "node_modules" || info.Name() == "bower_components") {
// 			log.Logger.Debugf("Skipping directory: %s", info.Name())
// 			return filepath.SkipDir
// 		}

// 		if !info.IsDir() && info.Name() == "package.json" {
// 			name := filepath.Base(filepath.Dir(path))
// 			// Parse from project name from `package.json` if possible
// 			if manifest, err := npm.ParseNodePackageManifest(path); err == nil {
// 				name = manifest.Name
// 			}

// 			log.Logger.Debugf("Found NodeJS project: %s (%s)", path, name)
// 			path, err = filepath.Rel(dir, path)
// 			if err != nil {
// 				log.Logger.Panicf("Could not construct NodeJS project path", err.Error())
// 			}
// 			programs = append(programs, project.Project{
// 				Name:        name,
// 				Type:        pkg.NodeJS,
// 				BuildTarget: path,
// 				Dir:         filepath.Dir(path),
// 			})
// 		}
// 		return nil
// 	})

// 	if err != nil {
// 		return nil, errors.Wrap(err, "could not find NodeJS projects")
// 	}

// 	return programs, nil
// }

// // Build runs `yarn install --production --frozen-lockfile` if there exists a
// // `yarn.lock` and `yarn` is available. Otherwise, it runs
// // `npm install --production`.
// func (n *NodeJSAnalyzer) Build(config project.Project) error {
// 	log.Logger.Debugf("Running Node.js build: %#v", config)

// 	// Prefer Yarn where possible
// 	if ok, err := files.Exists(config.Dir, "yarn.lock"); err == nil && ok && n.yarnOK {
// 		_, _, err := exec.Run(exec.Cmd{
// 			Name: n.yarnCmd,
// 			Argv: []string{"install", "--production", "--frozen-lockfile"},
// 			Dir:  config.Dir,
// 		})
// 		if err != nil {
// 			return errors.Wrap(err, "could not run `yarn` build")
// 		}
// 	} else if n.npmOK {
// 		_, _, err := exec.Run(exec.Cmd{
// 			Name: n.npmCmd,
// 			Argv: []string{"install", "--production"},
// 			Dir:  config.Dir,
// 		})
// 		if err != nil {
// 			return errors.Wrap(err, "could not run `npm` build")
// 		}
// 	} else {
// 		return ErrNoNPM
// 	}

// 	log.Logger.Debug("Done running Node.js build.")
// 	return nil
// }

// // IsBuilt returns true if a project has a manifest and either has no
// // dependencies or has a `node_modules` folder.
// //
// // Note that there could be very strange builds where this will produce false
// // negatives (e.g. `node_modules` exists in a parent folder). There can also
// // exist builds where this will produce false positives (e.g. `node_modules`
// // folder does not include the correct dependencies). We also don't take
// // $NODE_PATH into account during resolution.
// //
// // TODO: with significantly more effort, we can eliminate both of these
// // situations.
// func (n *NodeJSAnalyzer) IsBuilt(config project.Project) (bool, error) {
// 	log.Logger.Debugf("Checking Node.js build: %#v", config)

// 	manifest, err := npm.ParseNodePackageManifest(config.BuildTarget)
// 	if err != nil {
// 		return false, errors.Wrap(err, "could not parse package manifest to check build")
// 	}

// 	if len(manifest.Dependencies) > 0 {
// 		log.Logger.Debugf("Done checking Node.js build: project has no dependencies")
// 		return true, nil
// 	}

// 	hasNodeModules, err := files.ExistsFolder(config.Dir, "node_modules")
// 	if err != nil {
// 		return false, err
// 	}

// 	log.Logger.Debugf("Done checking Node.js build: %#v", hasNodeModules)
// 	return hasNodeModules, nil
// }

// func (n *NodeJSAnalyzer) Analyze(config project.Project) (project.Project, error) {
// 	log.Logger.Debugf("Running Nodejs analysis: %#v %#v", m, allowUnresolved)

// 	// TODO: we must allow this to exit with error if a flag is passed (maybe --allow-npm-err)
// 	// because sometimes npm will throw errors even after a complete install
// 	out, stderr, err := runLogged(m.Dir, builder.NPMCmd, "ls", "--json")
// 	if err != nil {
// 		log.Logger.Warningf("NPM had non-zero exit code: %s", stderr)
// 	}

// 	var parsed nodeListOutput
// 	err = json.Unmarshal([]byte(out), &parsed)
// 	if err != nil {
// 		return nil, errors.Wrap(err, "could not parse `npm ls --json` output")
// 	}
// 	imports := flattenNodeJSModules(parsed)
// 	deps := computeImportPaths(imports)

// 	log.Logger.Debugf("Done running Nodejs analysis: %#v", deps)
// 	return deps, nil
// 	return project.Project{}, analyzerutil.ErrNotImplemented
// }
