# pixi

[pixi](https://pixi.prefix.dev) is a workspace and package manager for conda and PyPI packages.

The pixi strategy uses static analysis. It does not require pixi to be installed.

## Project Discovery

Find files named `pixi.lock`. No `pixi.toml` or `pyproject.toml` is required.

## Analysis

We parse the YAML file `pixi.lock` and report packages from all its environments and platforms.
Conda packages use the same names as the [conda](conda.md) strategy. PyPI packages are reported as pip dependencies, and `git+` sources as git dependencies.
Local packages are reported as path dependencies. To include them in scans, enable [path dependency analysis](../../../experimental/path-dependency.md).

The `depends` and `requires_dist` fields supply dependency edges. Direct dependencies come from `pixi.toml` or `pyproject.toml` when present, and are otherwise inferred from the graph. Packages that nothing else depends on are treated as direct dependencies. All members of a dependency cycle are direct if nothing outside the cycle depends on them.
The workspace's own package, at `.` or `./`, is omitted. When it is direct, its dependencies become direct.

The `default` environment maps to production, `dev` and `development` to development, and `test` and `testing` to testing. Other environment names are preserved. Packages shared across environments carry all their environments.

Lockfile versions 5 through 7 are supported. Other versions are parsed with a warning.
Unreadable package entries, including `conda_source` packages, are skipped with warnings.
Analysis fails if the lockfile structure cannot be parsed, or if it lists packages other than the workspace itself but produces no dependencies. Empty and workspace-only lockfiles produce an empty graph.

## Limitations

- A PyPI requirement that only applies to some Python versions or platforms is treated as always applying.
- Conda edges stay within a subdir unless either package is `noarch` or local.
- Conda builds for different platforms remain separate dependencies. Shared `noarch` packages are deduplicated.
- Conda names and versions are read from artifact filenames when explicit fields are absent, as in lockfile versions 6 and 7.
