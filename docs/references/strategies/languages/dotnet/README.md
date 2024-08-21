# .NET Analysis

There are several different methods of .NET analysis, that use both the [NuGet](./nuget.md) (`nuspec`, `PackageReference`, `packages.config`, `project.json`, `project.assets.json`) and [Paket](./paket.md) package managers.

| Strategy                                 | Direct Deps | Transitive Deps | Edges |
| ---------------------------------------- | ----------- | --------- | ----- |
| [paket](paket.md)                           | ✅          | ✅        | ✅    |
| [project.assets.json](projectassetsjson.md) | ✅          | ✅        | ✅    |
| [PackageReference](packagereference.md)     | ✅          | ❌        | ❌    |
| [project.json](projectjson.md)              | ✅          | ❌        | ❌    |
| [packages.config](packagesconfig.md)        | ✅          | ❌        | ❌    |
| [nuspec](nuspec.md)                         | ✅          | ❌        | ❌    |