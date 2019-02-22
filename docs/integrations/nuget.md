# NuGet (.NET)

## Installation

NuGet support in FOSSA CLI depends on the following tools existing in your environment:

- [.NET Core CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/) (defaults to `dotnet`, configure with `$DOTNET_BINARY`)
- [NuGet](https://www.nuget.org/downloads) (defaults to `nuget`, configure with `$NUGET_BINARY`)

## Configuration

Automatic: Run `fossa init` to walk the file tree and find any directories that contain `packages.config` or `project.json`.

Manual: Add a module with `type` set to `nuget`, `dir` set to the location of the a `project.json` or `packages.config` file, and `target` to the path to that directory.

```yaml
analyze:
  modules:
    - name: NugetModule
      type: nuget
      dir: MyCompany.SomeProject.Module
      target: src/MyCompany.SomeProject.Module
```

## Analysis

`fossa analyze` will first attempt to resolve any existing NuGet lockfile created by your build (at `{module.path}/project.lock.json or {module.path}/obj/project.assets.json`).  It will parse these files for dependencies that were installed under the `libraries` key.  If `fossa` failed to resolve a lockfile (one was not created during the build or found), `fossa` will fall back to analyzing your `packages` directory.

## Known limitations

- doesn't support conditional direct dependencies (e.g. conditioned on target framework) -- will get all references regardless of target framework.
- only supports top-level itemgroup/packagereferences (not e.g. those under choose element).

- Currently, `fossa` supports NuGet lockfiles of `v2` and `v3` schemas.
- `fossa` assumes your package directory is located at `{module.path}/packages`.  If you use a global package folder or another path, we reccomend you generate a lockfile for your build..
- Due to the assumptions about package installation locations, verifying whether a module is built is unreliable sans-lockfile.  If you receive an inaccurate error that your build is unsatisfied, run `fossa` with the `--allow-unresolved` flag.
