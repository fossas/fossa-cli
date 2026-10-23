# pixi

[pixi](https://pixi.prefix.dev) is a workspace and package manager for conda and PyPI packages.

The pixi strategy uses static analysis. It does not require pixi to be installed.

## Project Discovery

Find files named `pixi.lock`. No `pixi.toml` or `pyproject.toml` is required.

## Analysis

We parse the YAML file `pixi.lock` and report packages from all its environments and platforms.
Conda packages use the same names as the [conda](conda.md) strategy. PyPI packages are reported as pip dependencies, and `git+` sources as git dependencies.
Local packages are reported as path dependencies. To include them in scans, enable [path dependency analysis](../../../experimental/path-dependency.md).

Direct dependencies come from `pixi.toml` or `pyproject.toml` when present, and are otherwise inferred from the graph.

The `default` environment is reported as production, because that's the conservative choice.

Lockfile versions 5 through 7 are supported. Other versions are parsed with a warning.
Packages pixi builds from source are reported as git, URL, or path dependencies, depending on where they're built from.
Analysis fails if the lockfile structure cannot be parsed, or if it lists packages other than the workspace itself but produces no dependencies. Empty and workspace-only lockfiles produce an empty graph.

## Limitations

- A PyPI requirement that only applies to some Python versions or platforms is treated as always applying.
- Conda edges stay within a subdir unless either package is `noarch` or local.
- Conda builds for different platforms remain separate dependencies. Shared `noarch` packages are deduplicated.
- Conda names and versions are read from artifact filenames when explicit fields are absent, as in lockfile versions 6 and 7.
