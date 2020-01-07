# .NET Analysis 

## Analysis 

### Strategies

| Name              | Transitive Deps | Direct Deps |   Tags   | Accuracy | Success Rate |
| ----------------- | :-------------: | :---------: | :------: | :------: | :----------: |
| Paket             |       Yes       |     Yes     | Registry |   100%   |     100%     |
| Package Reference |       Yes       |     Yes     | Registry |   100%   |     100%     |
| Nuspec            |       No        |     Yes     |   None   |   50%    |     100%     |
| packages.config   |       No        |     Yes     |   None   |   50%    |     100%     |
| project.json      |       No        |     Yes     |   None   |   50%    |     100%     |

#### [Paket](https://fsprojects.github.io/Paket/)

Paket is a third party dependency manager for .NET packages. Paket allows users to use NuGet packages, packages from Git repositories, and any other HTTP resource. Projects using Paket will contain three files, `paket.lock`, `package.dependencies`, and `paket.references`. `paket.dependencies` is present in all Paket projects and includes a list of dependencies with specs and their remotes:

```
source https://nuget.org/api/v2

nuget Castle.Windsor-log4net !~> 3.2
nuget Rx-Main !~> 2.0
```

`paket.lock` is generated after `paket install` is run and contains a fully resolved graph of all dependencies required for the project and their transitive dependencies.

```
NUGET
  remote: https://nuget.org/api/v2
    Castle.Core (3.3.0)
    Castle.Core-log4net (3.3.0)
      Castle.Core (>= 3.3.0)
      log4net (1.2.10)
    Castle.LoggingFacility (3.3.0)
      Castle.Core (>= 3.3.0)
      Castle.Windsor (>= 3.3.0)
    Castle.Windsor (3.3.0)
      Castle.Core (>= 3.3.0)
    Castle.Windsor-log4net (3.3.0)
      Castle.Core-log4net (>= 3.3.0)
      Castle.LoggingFacility (>= 3.3.0)
    Rx-Core (2.2.5)
      Rx-Interfaces (>= 2.2.5)
    Rx-Interfaces (2.2.5)
    Rx-Linq (2.2.5)
      Rx-Interfaces (>= 2.2.5)
      Rx-Core (>= 2.2.5)
    Rx-Main (2.2.5)
      Rx-Interfaces (>= 2.2.5)
      Rx-Core (>= 2.2.5)
      Rx-Linq (>= 2.2.5)
      Rx-PlatformServices (>= 2.2.5)
    Rx-PlatformServices (2.2.5)
      Rx-Interfaces (>= 2.2.5)
      Rx-Core (>= 2.2.5)
    log4net (1.2.10)
```
Paket recommends that users commit their `paket.lock` files to VCS for consistency, but this cannot be guaranteed. As a result, support for Paket will rely on detecting and scanning both `paket.dependencies` and `paket.lock`. If the results from `paket.lock` are found then the results from `paket.dependencies` can be disregarded.

#### [PackageReference](https://docs.microsoft.com/en-us/nuget/consume-packages/package-references-in-project-files)

> Note: By default, PackageReference is used for .NET Core projects, .NET Standard projects, and UWP projects targeting Windows 10 Build 15063 (Creators Update) and later, with the exception of C++ UWP projects. .NET Framework projects support PackageReference, but currently default to packages.config.

PackageReference files were introduced with NuGet 4.0 and are the current standard. They are an improvement over using `packages.config` and `project.json` to manage .NET dependencies. A project using the PackageReference format can contain many PackageReference files throughout the file tree which reference each other. After a project using PackageReference files is built, a lockfile named `project.assets.json` will be created at the root of the project and contain a fully resolved transitive dependency graph

Example `project.assets.json` file:
```json
{
      "version": 3,
      "targets": {
            ".NETFramework,Version=v4.0": {
                  "one/1.0.0": {
                        "type": "package",
                        "dependencies": {
                              "three": "3.0.0"
                        }
                  },
                  "two/2.0.0": {
                        "type": "package"
                  },
                  "three/3.0.0": {
                        "type": "package"
                  }
            }
      }
}
```

Analyzing `project.assets.json` file is the ideal analysis method for NuGet projects as it returns a complete dependency graph. In the event this file is not present, we can fall back to transitively analyzing the PackageReference files.

Example PackageReference file listing a single dependency:
```xml
<ItemGroup>
    <ProjectReference Include="..\transitive-file\transitive.csproj">
    </ProjectReference>
</ItemGroup>
<ItemGroup>
    <PackageReference Include="Contoso.Utility.UsefulStuff" Version="3.6.0" />
</ItemGroup>
```

Discovery for a PackageReference managed project looks for two types of files. First, the `project.assets.json` file is searched for and then PackageReference formatted files with the extensions of `.csproj`, `.xproj`,  `.vbproj`, `.dbproj`, or `.fsproj` are discovered.

Analysis for PackageReference files relies on parsing a root PackageReference file and transitively parsing the files it contains references to in the `ProjectReference` blocks. Each file is parsed for dependencies and the results are aggregated to return an accurate list of direct dependencies and their pinned versions. Transitive analysis can be run on these direct dependencies to find a more accurate dependency graph.

#### [Nuspec](https://docs.microsoft.com/en-us/nuget/reference/nuspec)

The `.nuspec` file is an old file that is used to list package metadata and contains dependency information. These files have become largely irrelevant in new .NET projects with the presence of the PackageReference files, however a significant amount of projects that exist continue to use this `.nuspec` file to specify dependencies.

The `.nuspec` file is XML formatted:

```xml
<?xml version="1.0"?>
<package xmlns="test">
  <metadata minClientVersion="2.12">
    <id>NUnit</id>
    <title>NUnit</title>
    <version>$version$</version>
    <dependencies>
      <group targetFramework="testone" />
      <group targetFramework="testtwo">
        <dependency id="one" version="1.0.0" />
        <dependency id="two" version="2.0.0" />
      </group>
      <group targetFramework="testthree">
        <dependency id="three" version="3.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
```

Analyzing `.nuspec` files is done by reading the `dependencies` block to find a list of direct dependencies and their pinned versions. Without a full transitive list of dependencies, transitive analysis can be run on the dependency graph once it is uploaded to obtain a more accurate graph.

This strategy is discovered anytime a file matches the `*.nuspec` pattern.

#### [packages.config](https://docs.microsoft.com/en-us/nuget/reference/packages-config)

The `packages.config` file is a file used to manage project dependencies and is the predecessor to the PackageReference file format. The file is XML formatted and the structure is straightforward:

```xml
<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="one" version="1.0.0" />
  <package id="two" version="2.0.0" />
</packages>
```

Analysis and discovery happens by detecting the existence of a `packages.config` file and extracting the dependency information from the file under the `packages` block.

#### [project.json](https://docs.microsoft.com/en-us/nuget/archive/project-json)

The `project.json` file was used to store dependency management and project metadata until NuGet 4.0 which was released in March 2017:

```JSON
{
      "version": "1",
      "compilationOptions": {
            "test": true
      },
      "dependencies": {
            "one": "1.0.0",
            "two": "2.0.0",
            "three": {
                  "version": "3.0.0",
                  "type": "test"
            }
      },
      "code": [
            "**/*.cs"
      ],
      "frameworks": {
            "dummy": {
                  "frameworkAssemblies": {
                        "fake": ""
                  }
            }
      }
}
```

Analysis and discovery happen by detecting the existence of a `project.json` file and extracting the dependency information from the file. All available dependency information is contained within this file.

## Alternatives

### [SLN files](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019)

A solution file maintains state information for a project and acts as a sort of lockfile. This file doesn't appear to offer any advantage of viewing the `project.assets.json` file which contains the locked dependencies. If the extra package metadata ever becomes interesting then this file may be interesting enough to analyze.