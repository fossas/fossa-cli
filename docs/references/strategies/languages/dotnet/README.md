# .NET Analysis

There are several different methods of .NET analysis, that use both the `NuGet` (`nuspec`, `PackageReference`, `packages.config`, `project.json`, `project.assets.json`) and `Paket` package managers.

| Strategy                                 | Direct Deps | Deep Deps | Edges |
| ---------------------------------------- | ----------- | --------- | ----- |
| [nuspec][nuspec]                         | ✅          | ❌        | ❌    |
| [PackageReference][packagereference]     | ✅          | ❌        | ❌    |
| [packages.config][packagesconfig]        | ✅          | ❌        | ❌    |
| [paket][paket]                           | ✅          | ✅        | ✅    |
| [project.assets.json][projectassetsjson] | ✅          | ✅        | ✅    |
| [project.json][projectjson]              | ✅          | ❌        | ❌    |

[nuspec](nuspec.md)
[packagereference](packagereference.md)
[packagesconfig](packagesconfig.md)
[paket](paket.md)
[projectassetsjson](projectassetsjson.md)
[projectjson](projectjson.md)