# .NET

## Installation

.NET support in FOSSA CLI only depends on the presence of .NET related manifest files in the directory.

## Configuration

Automatic: Run `fossa init` to walk the file tree and find any directories that contain a Package Reference file, NuSpec file, `packages.config`, `project.json`, or `paket.lock`.

Manual: Add a module with `type` set to `nuget`, `target` to the path of the manifest file, and `dir` set to the location of the manifest file.

```yaml
analyze:
  modules:
    - name: NugetModule
      type: nuget
      target: MyProject/Manifest.csproj
      path: MyProject
      options:
        strategy: package-reference
```

## Options

| Option     |  Type  | Name                         | Common Use Case                    |
| ---------- | :----: | ---------------------------- | ---------------------------------- |
| `strategy` | string | [Strategy](#strategy-string) | Specify a .NET analysis strategy. |

#### `strategy: <string>`

Manually specify the .NET analysis strategy to be used. Supported options:
- `paket`: Parse `paket.lock` file.
- `package-reference`: Parse a Package Reference file.
- `nuspec`: Parse a NuSpec file.
- `packages-config`: Parse `packages.config` file.
- `project-json`: Parse `project.json` file.

## Analysis

Default .NET analysis follows a series of fallbacks which attempts to determine a dependency graph by starting with the most accurate method and falling to the least accurate:

1. Paket: Look for `paket.lock` and read for dependencies.
2. Resolving method: Attempt to resolve any existing NuGet lockfile created by your build (at `{module.path}/obj/project.assets.json`). It will parse this file for installed dependencies and compare them to the Package Reference file in order to determine an accurate dependency graph.
3. Package Reference: Look for a file that matches the Package Reference file format and read for dependencies.
4. NuSpec: Look for a NuSpec file and read for dependencies.
5. Packages Config: Look for `packages.config` and read for dependencies.
6. Project JSON: Look for `project.json` and read for dependencies.

## Known limitations

- Doesn't support conditional direct dependencies (e.g. conditioned on target framework) -- will get all references regardless of target framework.
- Only supports top-level itemgroup/packagereferences (not e.g. those under choose element).
- Currently, `fossa` supports NuGet lockfiles of `v2` and `v3` schemas.
