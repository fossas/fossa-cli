# .NET Analysis

There are several different methods of .NET analysis, that use both the `NuGet` (`nuspec`, `PackageReference`, `packages.config`, `project.json`, `project.assets.json`) and `Paket` package managers.

| Strategy                                 | Direct Deps | Transitive Deps | Edges |
| ---------------------------------------- | ----------- | --------- | ----- |
| [nuspec](nuspec.md)                         | ✅          | ❌        | ❌    |
| [PackageReference](packagereference.md)     | ✅          | ❌        | ❌    |
| [packages.config](packagesconfig.md)        | ✅          | ❌        | ❌    |
| [paket](paket.md)                           | ✅          | ✅        | ✅    |
| [project.assets.json](projectassetsjson.md) | ✅          | ✅        | ✅    |
| [project.json](projectjson.md)              | ✅          | ❌        | ❌    |