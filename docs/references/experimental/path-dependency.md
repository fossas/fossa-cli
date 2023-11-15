# Path Dependency

## What is path dependency?

Path dependency is a reliance on the file system as the source, as opposed to a package manager registry or URL. A path dependency may or may not have transitive dependencies.

For example, in the following `go.mod` file, with `gomod` analysis and the `--experimental-analyze-path-dependencies` flag, `fossa-cli` would consider `../vendor/squirrel` a path dependency in the final dependency graph. If path dependency analysis is disabled, `fossa-cli` would ignore this dependency completely and only show transitive dependencies originating from `../vendor/squirrel`. In such a case, license and copyright obligations originating from `../vendor/squirrel` will not be captured in FOSSA's findings, and subsequent software bill of materials generated.

```go
// Example go.mod file
module tester

go 1.14

replace github.com/Masterminds/squirrel => ../vendor/squirrel
require github.com/Masterminds/squirrel v1.4.0
```

In the FOSSA UI, path dependencies are shown with the `Local` tag.

## How are path dependencies scanned?

Path dependencies are scanned using the "CLI license scan," similar to how [vendor dependencies](./../../features/vendored-dependencies.md) are scanned by default. A "CLI license scan" performs a license scan at the path and uploads the results of these scans to the provided FOSSA endpoint.

For performance reasons, the FOSSA service caches the results of a path dependency by the combination of its `(project id, path, hash of path's content)`. Due to this caching setup, it is normal for the first analysis to take some time, especially for larger projects, but future analyses of dependencies with the same information should be fast.

In the event that caching is causing problems, FOSSA can be made to rescan this kind of dependency by:
- Running `fossa analyze` with the `--force-vendored-dependency-rescans` flag, or
- Setting `vendoredDependencies.forceRescans` to `true` in `.fossa.yml` at the root of the project.

## Limitations

- Currently, path dependencies do not support vulnerability scanning.
- Currently, path dependencies are only supported in:
  - `golang` using the [gomod strategy](./../strategies/languages/golang/gomodules.md).
  - PDM (Python) projects via the [pdm strategy](./../strategies/languages/python/pdm.md).

## F.A.Q

### How do I enable path dependency in FOSSA analysis?

Run `fossa analyze` with the `--experimental-analyze-path-dependencies` flag.

### How do I disable path dependency in FOSSA analysis?

By default, path dependency analysis is disabled. Note that, in the future, `fossa-cli` will enable path dependency analysis by default.

### Is `fossa-cli` uploading the content of my path dependency to the server?

`fossa-cli` only uploads findings of the license scan result.

## How are path dependencies different from vendor dependencies?

Path dependencies, unlike [vendored dependencies](./../../features/vendored-dependencies.md), can be either direct or transitive dependencies in the dependency graph. Furthermore, path dependencies are scoped to the project in FOSSA, as opposed to the entire organization.