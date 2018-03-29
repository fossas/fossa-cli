package builders

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar"
	logging "github.com/op/go-logging"

	"github.com/fossas/fossa-cli/module"
)

var nugetLogger = logging.MustGetLogger("nuget")

// NuGetPackage implements Dependency for NuGet
type NuGetPackage struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Fetcher always returns gem for NuGetPackage
func (m NuGetPackage) Fetcher() string {
	return "nuget"
}

// Package returns the package spec for NuGetPackage
func (m NuGetPackage) Package() string {
	return m.Name
}

// Revision returns the version spec for NuGetPackage
func (m NuGetPackage) Revision() string {
	return m.Version
}

type nuGetLockfileV3 struct {
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
	nuGetCmd, nuGetVersonOut, err := which("help", os.Getenv("NUGET_BINARY"), "nuget")
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

// Analyze parses the output of NuGet lockfiles and falls back to parsing the packages folder
func (builder *NuGetBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	nugetLogger.Debugf("Running NuGet analysis: %#v %#v", m, allowUnresolved)

	deps := []module.Dependency{}

	// Find and parse a lockfile
	lockFilePath, err := resolveNuGetProjectLockfile(m.Dir)
	if err == nil {
		var lockFile nuGetLockfileV3
		if err := parseLogged(nugetLogger, lockFilePath, &lockFile); err == nil {
			for depKey := range lockFile.Libraries {
				depKeyParts := strings.Split(depKey, "/")
				if len(depKeyParts) == 2 {
					deps = append(deps, module.Dependency(NuGetPackage{
						Name:    depKeyParts[0],
						Version: depKeyParts[1],
					}))
				}
			}
		}
	} else {
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
				deps = append(deps, module.Dependency(NuGetPackage{
					Name:    match[1],
					Version: match[3],
				}))
			}
		}
	}

	// TODO: filter out system deps

	nugetLogger.Debugf("Done running NuGet analysis: %#v", deps)
	return deps, nil
}

// IsBuilt checks the existance of a project.lock.json or a packages directory
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
