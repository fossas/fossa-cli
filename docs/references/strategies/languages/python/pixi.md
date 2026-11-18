# pixi

[pixi](https://pixi.prefix.dev) is a workspace and package manager for conda and PyPI packages.

The pixi strategy uses static analysis. It does not require pixi to be installed.

## Project Discovery

Find files named `pixi.lock`. No `pixi.toml` or `pyproject.toml` is required.

## Analysis

We parse the YAML file `pixi.lock` and report packages from all its environments and platforms.
Conda packages use the same names as the [conda](conda.md) strategy, which include the platform, so builds of a package for different platforms are separate dependencies, while a `noarch` package is one dependency on every platform. In lockfile versions 6 and 7, the name and version of a conda package from a channel are read from its file name.
PyPI packages are reported as pip dependencies, and `git+` sources as git dependencies.
Local packages are reported as path dependencies. To include them in scans, enable [path dependency analysis](../../../experimental/path-dependency.md).

Direct dependencies are the packages declared in `pixi.toml` or `pyproject.toml`, including conda packages that pixi installs to satisfy declared PyPI requirements. Packages that nothing else depends on are also reported as direct, even with a manifest.

The `default` environment is reported as production, because that's the conservative choice. Environments named `dev` or `development` are reported as development, and `test` or `testing` as testing. As with other strategies' development and test dependencies, packages used only in those environments are left out unless you pass [`--include-unused-deps`](../../../subcommands/analyze.md). Packages in other environments are included.

Lockfile versions 4 through 7 are supported; 7 is the latest as of October 2026. Versions 1 through 3 use an older layout that pixi stopped writing in early 2024 and aren't supported.
Packages pixi builds from source are reported as git, URL, or path dependencies, depending on where they're built from.
Analysis fails if the lockfile structure cannot be parsed, or if its environments list packages other than the workspace itself but none of them produce a dependency. Empty and workspace-only lockfiles produce an empty graph.

## Limitations

- A PyPI requirement that only applies to some Python versions or platforms is treated as always applying.
