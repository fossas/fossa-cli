# .NET Analysis

There are several different methods of .NET analysis, that use both the `NuGet` (`nuspec`, `PackageReference`, `packages.config`, `project.json`, `project.assets.json`) and `Paket` package managers.

| Strategy                                    | Direct Deps        | Transitive Deps    | Edges              |
|---------------------------------------------|--------------------|--------------------|--------------------|
| [nuspec](nuspec.md)                         | :white_check_mark: | :x:                | :x:                |
| [PackageReference](packagereference.md)     | :white_check_mark: | :x:                | :x:                |
| [packages.config](packagesconfig.md)        | :white_check_mark: | :x:                | :x:                |
| [paket](paket.md)                           | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [project.assets.json](projectassetsjson.md) | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [project.json](projectjson.md)              | :white_check_mark: | :x:                | :x:                |
