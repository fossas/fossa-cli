# NuGet Analysis

| Strategy                                    | Direct Deps | Transitive Deps | Edges |
|---------------------------------------------|-------------|-----------------|-------|
| [project.assets.json](projectassetsjson.md) | ✅           | ✅               | ✅     |
| [PackageReference](packagereference.md)     | ✅           | ❌               | ❌     |
| [project.json](projectjson.md)              | ✅           | ❌               | ❌     |
| [packages.config](packagesconfig.md)        | ✅           | ❌               | ❌     |
| [nuspec](nuspec.md)                         | ✅           | ❌               | ❌     |

NuGet analysis follows these strategies in sequence:
1. `project.assets.json`
2. `PackageReference`

`project.assets.json` files are generated from `.csproj` files by `dotnet restore` and written to the project's `obj/` directory. `PackageReference` dependencies can be found in `.csproj`, `.xproj`, `.vbproj`, `.dbproj`, or `.fsproj` files. To consolidate findings from these two strategies, a project file is skipped when `obj/project.assets.json` exists beside it (or a `project.assets.json` sits in the same directory); `PackageReference` analysis is only used for projects that have not been restored. Because the assets file wins, run `dotnet restore` before `fossa analyze` so it reflects the current project file.

The following strategies are executed independently::
1. `project.json`
2. `packages.config`
3. `nuspec`

`project.json` and `packages.config` files are deprecated in favor of `.csproj` and their usage of `PackageReference` format. `nuspec` serves as a manifest containing package metadata, used both for building the package and providing information to consumers. These strategies are isolated from the `project.assets.json` and `PackageReference` approaches and therefore run independently.
