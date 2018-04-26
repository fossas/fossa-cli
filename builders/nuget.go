package builders

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
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
	// TODO: let the user configure a target to use.
	Targets map[string]map[string]struct {
		Dependencies map[string]string
	}
}

// NuGetBuilder implements Builder for Bundler (Gemfile) builds
type NuGetBuilder struct {
	DotNETCmd     string
	DotNETVersion string
	NuGetCmd      string
	NuGetVersion  string
}

// Initialize collects metadata on NuGet and NuGet environments
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

type dotNETProjectNode struct {
	name       string
	version    string
	file       string
	references []dotNETProjectNode
}

type dotNETProject struct {
	PropertyGroup []dotNETPropertyGroup
	ItemGroup     []dotNETItemGroup
}

type dotNETPropertyGroup struct {
	RootNamespace string
	Version       string
}

type dotNETItemGroup struct {
	PackageReference []dotNETPackageReference
}

type dotNETPackageReference struct {
	Include string `xml:",attr"`
	Version string `xml:",attr"`
}

// Given a starting `*.*proj` file, construct a graph of project references
func computeDotNETProjectGraph(cmd string, rootProjectFile string) (dotNETProjectNode, error) {
	nugetLogger.Debugf("Computing project graph from: %s", rootProjectFile)

	// Read file to get project reference name
	projectFileContents, err := ioutil.ReadFile(rootProjectFile)
	if err != nil {
		return dotNETProjectNode{}, errors.Wrap(err, "could not read NuGet project file")
	}
	var projectFileXML dotNETProject
	err = xml.Unmarshal(projectFileContents, &projectFileXML)
	if err != nil {
		return dotNETProjectNode{}, errors.Wrap(err, "could not parse NuGet project file")
	}
	name := rootProjectFile
	if len(projectFileXML.PropertyGroup) > 0 && projectFileXML.PropertyGroup[0].RootNamespace != "" {
		name = projectFileXML.PropertyGroup[0].RootNamespace
	}
	version := ""
	if len(projectFileXML.PropertyGroup) > 0 && projectFileXML.PropertyGroup[0].Version != "" {
		version = projectFileXML.PropertyGroup[0].Version
	}
	root := dotNETProjectNode{
		name:       name,
		version:    version,
		file:       rootProjectFile,
		references: nil,
	}

	// Get transitive project references
	refCmd := exec.Command(cmd, "list", rootProjectFile, "reference")
	refCmd.Env = os.Environ()
	refCmd.Env = append(refCmd.Env, "TERM=dumb")
	refOutBytes, err := refCmd.Output()
	refOut := string(refOutBytes)
	if err != nil {
		return dotNETProjectNode{}, errors.Wrapf(err, "could not run `%s list reference`", cmd)
	}
	header := "Project reference(s)\n--------------------\n"
	headerIndex := strings.Index(refOut, header)
	if headerIndex == -1 {
		return root, nil
	}
	projects := refOut[headerIndex+len(header):]
	for _, line := range strings.Split(projects, "\n") {
		if line == "" {
			break
		}
		projectReference := filepath.Join(filepath.Dir(rootProjectFile), filepath.Join(strings.Split(line, "\\")...))
		transitive, err := computeDotNETProjectGraph(cmd, projectReference)
		if err != nil {
			errors.Wrap(err, "could not get transitive project files")
		}
		root.references = append(root.references, transitive)
	}

	return root, nil
}

type dotNETPackageNode struct {
	Name         string
	Version      string
	NodeType     string // project or package
	Dependencies map[string]dotNETPackageNode
}

// Given a project references graph, construct a package graph with direct dependencies.
func createDotNETPackageGraph(rootProjectNode dotNETProjectNode) (dotNETPackageNode, error) {
	nugetLogger.Debugf("Creating package graph from: %#v", rootProjectNode)

	// Read file to get direct dependencies
	projectFileContents, err := ioutil.ReadFile(rootProjectNode.file)
	if err != nil {
		return dotNETPackageNode{}, errors.Wrap(err, "could not read NuGet project file")
	}
	var projectFileXML dotNETProject
	err = xml.Unmarshal(projectFileContents, &projectFileXML)
	if err != nil {
		return dotNETPackageNode{}, errors.Wrap(err, "could not parse NuGet project file")
	}
	root := dotNETPackageNode{
		Name:         rootProjectNode.name,
		Version:      rootProjectNode.version,
		NodeType:     "project",
		Dependencies: make(map[string]dotNETPackageNode),
	}

	// Get direct package references
	for _, group := range projectFileXML.ItemGroup {
		for _, packageRef := range group.PackageReference {
			root.Dependencies[packageRef.Include] = dotNETPackageNode{
				Name:         packageRef.Include,
				Version:      packageRef.Version,
				NodeType:     "package",
				Dependencies: make(map[string]dotNETPackageNode),
			}
		}
	}

	// Resolve transitive project references
	for _, ref := range rootProjectNode.references {
		pkgGraph, err := createDotNETPackageGraph(ref)
		if err != nil {
			return dotNETPackageNode{}, err
		}
		root.Dependencies[ref.name] = pkgGraph
	}

	return root, nil
}

// Given a package graph with direct dependencies, recursively add transitive dependencies and resolve versions from the lockfile.
func hydrateDotNETPackageGraph(rootPackageNode dotNETPackageNode, edges map[string]map[string]bool, versions map[string]string) dotNETPackageNode {
	// Hydrate version.
	resolvedVersion, ok := versions[rootPackageNode.Name]
	if !ok || rootPackageNode.Version != "" {
		resolvedVersion = rootPackageNode.Version
	}

	// Hydrate dependencies.
	// WARNING: this will infinitely loop if the dependency graph has cycles.
	hydratedEdges := make(map[string]dotNETPackageNode)
	for name, dep := range rootPackageNode.Dependencies {
		hydratedEdges[name] = hydrateDotNETPackageGraph(dep, edges, versions)
	}
	for edge := range edges[rootPackageNode.Name] {
		if _, ok := hydratedEdges[edge]; !ok {
			hydratedEdges[edge] = hydrateDotNETPackageGraph(dotNETPackageNode{
				Name:         edge,
				Version:      versions[edge],
				NodeType:     "package",
				Dependencies: make(map[string]dotNETPackageNode),
			}, edges, versions)
		}
	}

	return dotNETPackageNode{
		Name:         rootPackageNode.Name,
		Version:      resolvedVersion,
		NodeType:     rootPackageNode.NodeType,
		Dependencies: hydratedEdges,
	}
}

func flattenDotNETPackageGraphRecurse(pkg dotNETPackageNode, from module.ImportPath) []Imported {
	var imports []Imported
	locator := module.Locator{
		Fetcher:  "nuget",
		Project:  pkg.Name,
		Revision: pkg.Version,
	}
	for _, dep := range pkg.Dependencies {
		transitive := flattenDotNETPackageGraphRecurse(dep, append(from, locator))
		imports = append(imports, transitive...)
	}
	imports = append(imports, Imported{
		Locator: locator,
		From:    append(module.ImportPath{}, from...),
	})
	return imports
}

func flattenDotNETPackageGraph(pkg dotNETPackageNode) []Imported {
	return flattenDotNETPackageGraphRecurse(pkg, module.ImportPath{})
}

// Analyze parses the output of NuGet lockfiles and falls back to parsing the packages folder
func (builder *NuGetBuilder) Analyze(m module.Module, allowUnresolved bool) ([]module.Dependency, error) {
	nugetLogger.Debugf("Running NuGet analysis: %#v %#v", m, allowUnresolved)
	var deps []module.Dependency

	// Find and parse a lockfile
	lockFilePath, err := resolveNuGetProjectLockfile(m.Dir)

	if err == nil {
		// Get starting project file
		rootProject := m.Target

		// Compute project graph
		projectGraph, err := computeDotNETProjectGraph(builder.DotNETCmd, rootProject)
		if err != nil {
			return nil, errors.Wrap(err, "could not compute NuGet project graph")
		}

		// Construct direct package graph
		directPackageGraph, err := createDotNETPackageGraph(projectGraph)
		if err != nil {
			return nil, errors.Wrap(err, "could not compute NuGet package graph")
		}

		// Parse lockfile: get dependency graph edges and resolved versions.
		var lockFile nuGetLockfileV2or3
		err = parseLogged(nugetLogger, lockFilePath, &lockFile)
		if err != nil {
			return nil, errors.Wrap(err, "could not parse NuGet lockfile")
		}
		edges := make(map[string]map[string]bool)
		versions := make(map[string]string)
		for _, deps := range lockFile.Targets {
			for pkgKey, pkg := range deps {
				pkgKeyParts := strings.Split(pkgKey, "/")
				pkgName := pkgKeyParts[0]
				pkgVersion := pkgKeyParts[1]
				versions[pkgName] = pkgVersion
				for depName := range pkg.Dependencies {
					_, ok := edges[pkgName]
					if !ok {
						edges[pkgName] = make(map[string]bool)
					}
					edges[pkgName][depName] = true
				}
			}
		}

		// Hydrate package graph with edges and versions.
		packageGraph := hydrateDotNETPackageGraph(directPackageGraph, edges, versions)

		// Flatten package graph
		imports := flattenDotNETPackageGraph(packageGraph)

		// Compute import paths
		deps = computeImportPaths(imports)
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
