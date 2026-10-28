# Path Dependency

## What is path dependency?

Path dependency is a dependency, which is sourced from filesystem, as opposed to a package manager registry or URL. A path dependency may or may not have transitive dependencies. 
Path dependency is also referred to as `local` or `vendor` dependency by some package managers.

For example, in the following `go.mod` file, with `gomod` analysis and the `--experimental-analyze-path-dependencies` flag, `fossa-cli` would consider `../vendor/squirrel`, to be a path dependency. If path dependency analysis is disabled, `fossa-cli` would ignore this dependency completely and only show transitive dependencies originating from `../vendor/squirrel`. In such a case, license and copyright obligations originating from `../vendor/squirrel` will not be captured in FOSSA's findings, and subsequent software bill of materials generated.

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
- Currently, only path dependencies that point at a directory are license scanned; path dependencies that point at a single file are not supported and are reported as unscanned.
- Currently, path dependencies are only supported in:
  - `golang` using the [gomod strategy](./../strategies/languages/golang/gomodules.md).
  - PDM (Python) projects via the [pdm strategy](./../strategies/languages/python/pdm.md).
  - Rust projects via the [Cargo strategy](./../strategies/languages/rust/cargo.md).
  - Python projects using [uv](./../strategies/languages/python/uv.md).
  - Python projects using [Poetry](./../strategies/languages/python/poetry.md) (path dependencies in `pyproject.toml` and `poetry.lock`).
  - Swift/SwiftPM projects via the [Swift strategy](./../strategies/platforms/ios/swift.md).
  - Dart projects via the [Dart strategy](./../strategies/languages/dart/dart.md).

## Ecosystem support

Path dependency support has three layers, and a given ecosystem may stop at any of them:

1. **Concept**: the package manager lets a manifest or lockfile point at a directory on disk.
2. **Parsed**: the CLI's strategy recognizes the path reference when reading the manifest or lockfile.
3. **Analyzed**: the CLI treats it as a path dependency, i.e. resolves it on disk, license scans it, and reports the results as described above.

| Ecosystem | Path dep concept | Parsed by CLI | Analyzed as path dep |
| --- | --- | --- | --- |
| [Go](https://go.dev/ref/mod#go-mod-file-replace) (gomod strategy) | ✅ | ✅ | ✅ |
| [Cargo (Rust)](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#specifying-path-dependencies) | ✅ | ✅ | ✅ |
| [PDM (Python)](https://pdm-project.org/latest/usage/dependency/) | ✅ | ✅ | ✅ |
| [uv (Python)](https://docs.astral.sh/uv/concepts/projects/dependencies/#path-dependencies) | ✅ | ✅ | ✅ |
| [Poetry (Python)](https://python-poetry.org/docs/dependency-specification/#local-dependencies) | ✅ | ✅ | ✅ |
| [SwiftPM](https://developer.apple.com/documentation/packagedescription/package/dependency/package(name:path:)) | ✅ | ✅ | ✅ |
| [Dart / pub](https://dart.dev/tools/pub/dependencies#path-dependencies) | ✅ | ✅ | ✅ |
| [npm](https://docs.npmjs.com/cli/v10/configuring-npm/package-json#local-paths) | ✅ | ✅ | ❌ skipped |
| [Yarn 2+](https://yarnpkg.com/protocol/file) (`file:` protocol) | ✅ | ✅ | ❌ unsupported resolver |
| [pnpm](https://pnpm.io/lockfile) (`directory:` resolution) | ✅ | ✅ | ❌ reported as plain version |
| [Bun](https://bun.com/docs/install/lockfile) (`file:` resolution) | ✅ | ✅ | ❌ reported as plain version |
| [CocoaPods](https://guides.cocoapods.org/using/the-podfile.html) (`:path`) | ✅ | ✅ | ⚠️ podspec is read from disk for metadata, but the path is never license scanned |
| [pip](https://pip.pypa.io/en/stable/reference/requirements-file-format/) (requirements.txt paths) | ✅ | ✅ | ❌ skipped |
| [Bundler](https://bundler.io/man/gemfile.5.html) (`path:`) | ✅ | ❌ | ❌ |
| [Composer](https://getcomposer.org/doc/05-repositories.md#path) (path repositories) | ✅ | ❌ | ❌ |
| [Mix (Elixir)](https://hexdocs.pm/mix/Mix.Tasks.Deps.html) (`path:`) | ✅ | ❌ | ❌ |
| [Yarn 1](https://classic.yarnpkg.com/en/docs/package-json/) | ✅ | ❌ | ❌ |
| Pipenv | ✅ | ❌ | ❌ |
| Maven, Gradle, NuGet, Conan, etc. | ❌ | n/a | n/a |

## F.A.Q

### How do I enable path dependency in FOSSA analysis?

Run `fossa analyze` with the `--experimental-analyze-path-dependencies` flag.

### How do I disable path dependency in FOSSA analysis?

By default, path dependency analysis is disabled. Note that, in the future, `fossa-cli` will enable path dependency analysis by default.

### Is `fossa-cli` uploading the content of my path dependency to the server?

`fossa-cli` only uploads the portions of the file that contain matches to licenses, not the entire file. If `cliLicenseScanRequireFullFileUpload` is enabled in FOSSA endpoint
for your organization, `fossa-cli` will upload the full contents of any file that has licensing data. By default,  `cliLicenseScanRequireFullFileUpload` is
disabled for all organizations. Please contact [FOSSA support](https://support.fossa.com) to enable, this functionality.

## How are path dependencies different from FOSSA's vendor dependencies?

Path dependencies, unlike [vendored dependencies](./../../features/vendored-dependencies.md), can be either direct or transitive in the dependency graph. Furthermore, path dependencies are scoped to the project in FOSSA, as opposed to the entire organization.
