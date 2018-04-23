package builders

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"
	"github.com/pkg/errors"

	"github.com/fossas/fossa-cli/module"
)

var nugetLogger = logging.MustGetLogger("nuget")

type nuGetLockfileV2or3 struct {
	Targets map[string]struct {
		Dependencies map[string]string
	}
	Libraries map[string]struct{} `json:"libraries"`
}

// NuGetBuilder implements Builder for Bundler (Gemfile) builds
type NuGetBuilder struct {
	DotNETCmd     string
	DotNETVersion string
	NuGetCmd      string
	NuGetVersion  string
}

// Initialize collects metadata on NuGet and .NET environments
func (builder *NuGetBuilder) Initialize() error {
	nugetLogger.Debug("Initializing NuGet builder...")

	// Set DotNET context variables
	dotNetCmd, dotNetVersion, err := which("--version", os.Getenv("DOTNET_BINARY"), "dotnet")
	if err != nil {
		nugetLogger.Warningf("Could not find `dotnet` binary (try setting $DOTNET_BINARY): %s", err.Error())
	}
	builder.DotNETCmd = dotNetCmd
	builder.DotNETVersion = strings.TrimRight(dotNetVersion, "\n")

	// Set NuGet context variables
	// `nuget` hangs on Linux. Not sure why.
	// TODO: TEST: we should really add a timeout to all of these.
	// nuGetCmd, nuGetVersonOut, err := which("help", os.Getenv("NUGET_BINARY"), "nuget")
	nuGetCmd, nuGetVersonOut, err := which("help", os.Getenv("NUGET_BINARY"))
	if err == nil {
		builder.NuGetCmd = nuGetCmd

		nuGetVersionMatchRe := regexp.MustCompile(`NuGet Version: ([0-9]+\.[0-9]+\.[0-9]+.\w+)`)
		match := nuGetVersionMatchRe.FindStringSubmatch(nuGetVersonOut)
		if len(match) == 2 {
			builder.NuGetVersion = match[1]
		}
	} else {
		nugetLogger.Warningf("Could not find NuGet binary (try setting $NUGET_BINARY): %s", err.Error())
	}

	nugetLogger.Debugf("Initialized NuGet builder: %#v", builder)
	return nil
}

// Build runs `dotnet restore` and falls back to `nuget restore`
func (builder *NuGetBuilder) Build(m module.Module, force bool) error {
	nugetLogger.Debugf("Running NuGet build: %#v %#v", m, force)

	if builder.DotNETCmd != "" {
		dotNetSuccessKey := "Restore completed"
		dotNetStdout, dotNetStderr, err := runLogged(nugetLogger, m.Dir, builder.DotNETCmd, "restore")
		if err == nil && (strings.Contains(dotNetStdout, dotNetSuccessKey) || strings.Contains(dotNetStderr, dotNetSuccessKey)) {
			nugetLogger.Debug("NuGet build succeeded with `dotnet restore`.")
			return nil
		}
	}

	nugetLogger.Debug("`dotnet restore` did not succeed, falling back to `nuget restore`")

	if builder.NuGetCmd != "" {
		pkgDir, _ := resolveNugetPackagesDir(m.Dir)
		_, _, err := runLogged(nugetLogger, m.Dir, builder.NuGetCmd, "restore", "-PackagesDirectory", pkgDir)
		if err != nil {
			return fmt.Errorf("could not run `nuget install`: %s", err.Error())
		}
	} else {
		return errors.New("No tools installed in local environment for NuGet build")
	}

	nugetLogger.Debug("Done running NuGet build.")
	return nil
}

func (builder *NuGetBuilder) getNugetProjectFiles(file string) ([]string, error) {
	nugetLogger.Debugf("Getting transitive project files from: %s", file)
	files := []string{file}

	// Get transitive files
	refCmd := exec.Command(builder.DotNETCmd, "list", file, "reference")
	refCmd.Env = os.Environ()
	refCmd.Env = append(refCmd.Env, "TERM=dumb")
	refOutBytes, err := refCmd.Output()
	refOut := string(refOutBytes)
	if err != nil {
		return nil, errors.Wrapf(err, "could not run `%s list reference`", builder.DotNETCmd)
	}
	header := "Project reference(s)\n--------------------\n"
	headerIndex := strings.Index(refOut, header)
	if headerIndex == -1 {
		return files, nil
	}
	projects := refOut[headerIndex+len(header):]
	for _, line := range strings.Split(projects, "\n") {
		log.Printf("line: %s", line)
		if line == "" {
			break
		}
		parentFile := filepath.Join(filepath.Dir(file), filepath.Join(strings.Split(line, "\\")...))
		log.Printf("file: %s", parentFile)
		transitive, err := builder.getNugetProjectFiles(parentFile)
		if err != nil {
			errors.Wrap(err, "could not get transitive project files")
		}
		files = append(files, transitive...)
	}

	return files, nil
}

type dotNETProjectNode struct {
	name       string
	version    string
	references map[string]dotNETProjectNode
}

type dotNETPackageNode struct {
	name         string
	version      string
	nodeType     string // project or package
	dependencies map[string]dotNETPackageNode
}

// Analyze parses the output of NuGet lockfiles and falls back to parsing the packages folder
func (builder *NuGetBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	nugetLogger.Debugf("Running NuGet analysis: %#v %#v", m, allowUnresolved)
	var deps []module.Dependency

	// Find and parse a lockfile
	lockFilePath, err := resolveNuGetProjectLockfile(m.Dir)

	if err == nil {
		// // Get starting project file
		// rootProject := m.Target

		// // Compute project graph
		// projectGraph, err := computeDotNETProjectGraph(builder.DotNETCmd, rootProject)
		// if err != nil {
		// 	return nil, errors.Wrap(err, "could not compute .NET project graph")
		// }

		// // Compute package graph
		// packageGraph, err := computeDotNETPackageGraph(projectGraph)
		// if err != nil {
		// 	return nil, errors.Wrap(err, "could not compute .NET package graph")
		// }

		// // Flatten package graph
		// imports := flattenDotNETPackageGraph(packageGraph)

		// // Compute import paths
		// deps := computeImportPaths(imports)

		// Get all project files
		// TODO: we should probably have the module target specify a _project file_ instead of a directory
		// Get project files in current directory
		dirProjectFiles, err := filepath.Glob("*.*proj")
		if err != nil {
			return nil, errors.Wrap(err, "could not get project files in directory")
		}
		for i, file := range dirProjectFiles {
			dirProjectFiles[i], err = filepath.Abs(file)
			if err != nil {
				return nil, errors.Wrap(err, "could not resolve absolute path for project file")
			}
		}
		// Get transitive included project files
		var projectFiles []string
		for _, file := range dirProjectFiles {
			transitive, err := builder.getNugetProjectFiles(file)
			if err != nil {
				return nil, errors.Wrapf(err, "could not get project files")
			}
			projectFiles = append(projectFiles, transitive...)
		}
		log.Println(projectFiles)

		// Parse project files for direct dependencies
		// Parse cache lockfile for dependency graph edges
		var lockFile nuGetLockfileV2or3
		if err := parseLogged(nugetLogger, lockFilePath, &lockFile); err == nil {
			for depKey := range lockFile.Libraries {
				depKeyParts := strings.Split(depKey, "/")
				if len(depKeyParts) == 2 {
					deps = append(deps, module.Dependency{
						Locator: module.Locator{
							Fetcher:  "nuget",
							Project:  depKeyParts[0],
							Revision: depKeyParts[1],
						},
						Via: nil,
					})
				}
			}
		}
	} else {
		// TODO: test this code path with a fixture
		// Fallback to parsing the packages directory
		packagesDir, err := resolveNugetPackagesDir(m.Dir)

		nugetLogger.Debugf("No lockfile found; parsing packages directory: %s", packagesDir)
		if exists, err := hasFile(packagesDir); err != nil || !exists {
			return nil, fmt.Errorf("Unable to verify packages directory: %s", packagesDir)
		}

		packagePaths, err := ioutil.ReadDir(packagesDir)
		if err != nil {
			return nil, err
		}

		for _, f := range packagePaths {
			packageNameRe := regexp.MustCompile(`(([A-z]+\.?)+)\.(([0-9]+\.)+[\w-]+)`)
			match := packageNameRe.FindStringSubmatch(f.Name())
			nugetLogger.Debugf("%s, %V", len(match), match)
			if len(match) == 5 {
				deps = append(deps, module.Dependency{
					Locator: module.Locator{
						Fetcher:  "nuget",
						Project:  match[1],
						Revision: match[3],
					},
					Via: nil,
				})
			}
		}
	}

	// TODO: filter out system deps

	nugetLogger.Debugf("Done running NuGet analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks the existance of a lockfile or a packages directory
func (builder *NuGetBuilder) IsBuilt(m module.Module, allowUnresolved bool) (bool, error) {
	if allowUnresolved {
		return true, nil
	}

	nugetLogger.Debug("Checking NuGet module directory for a project lockfile")
	if _, err := resolveNuGetProjectLockfile(m.Dir); err != nil {
		nugetLogger.Debug("Checking NuGet packages directory for existence")

		packagesDir, _ := resolveNugetPackagesDir(m.Dir)
		return hasFile(packagesDir)
	}

	return true, nil
}

// IsModule is not implemented
func (builder *NuGetBuilder) IsModule(target string) (bool, error) {
	return false, errors.New("IsModule is not implemented for NuGetBuilder")
}

// resolveNugetPackagesDir parses a NuGet module config and resolves it to an existing package directory
func resolveNugetPackagesDir(dir string) (string, error) {
	packagesDir := filepath.Join(dir, "packages")
	return packagesDir, fmt.Errorf("unable to resolve NuGet packages directory: %s", "Not Implemented.")
}

func resolveNuGetProjectLockfile(dir string) (string, error) {
	lockfilePathCandidates := []string{"project.lock.json", "obj/project.assets.json"}
	for _, path := range lockfilePathCandidates {
		nugetLogger.Debugf("Checking for lockfile: %s/%s", dir, path)
		if hasLockfile, err := hasFile(dir, path); hasLockfile && err == nil {
			return filepath.Join(dir, path), nil
		}
	}

	return "", fmt.Errorf("No lockfiles detected in directory root: %s", dir)
}

// DiscoverModules returns ModuleConfigs that match `packages.config` in the directory
func (builder *NuGetBuilder) DiscoverModules(dir string) ([]module.Config, error) {
	packageRecordPaths, err := doublestar.Glob(filepath.Join(dir, "**", "{*.csproj,*.xproj,*.vbproj,*.dbproj,*.fsproj,packages.config,project.json,*.nuspec}"))
	if err != nil {
		return nil, err
	}
	moduleConfigs := make([]module.Config, 0)
	for _, path := range packageRecordPaths {
		packageName := filepath.Base(filepath.Dir(path))
		// infer title from *.nuspec in directory if exists
		nuSpecs, err := doublestar.Glob(filepath.Join(filepath.Dir(path), "*.nuspec"))
		if err == nil && len(nuSpecs) > 0 {
			packageName = strings.TrimRight(filepath.Base(nuSpecs[0]), ".nuspec")
		}
		path, _ := filepath.Rel(dir, path)
		packagePath := filepath.Dir(path)

		seen := false
		for _, m := range moduleConfigs {
			if m.Name == packageName && m.Path == packagePath {
				seen = true
				break
			}
		}
		if !seen {
			moduleConfigs = append(moduleConfigs, module.Config{
				Name: packageName,
				Path: packagePath,
				Type: "nuget",
			})
		}
	}

	return moduleConfigs, nil
}
