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

`project.assets.json` files and their dependencies are generated from  `.csproj` files. `PackageReference` dependencies can be found in `.csproj`, `.xproj`, `.vbproj`, `.dbproj`, or `.fsproj` files. To consolidate findings from these two strategies, `project.assets.json` analysis is attempted first and falls back to `PackageReference` analysis. 

The following strategies are executed independently::
1. `project.json`
2. `packages.config`
3. `nuspec`

`project.json` and `packages.config` files are deprecated in favor of `.csproj` and their usage of `PackageReference` format. `nuspec` serves as a manifest containing package metadata, used both for building the package and providing information to consumers. These strategies are isolated from the `project.assets.json` and `PackageReference` approaches and therefore run independently.
