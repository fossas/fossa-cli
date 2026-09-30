# pixi

[pixi](https://pixi.prefix.dev) is a workspace and package manager built on the conda ecosystem. A pixi workspace can mix conda packages and PyPI packages in a single lockfile, and resolves them for every environment and platform the workspace declares.

The pixi strategy is a static analysis strategy and does not require the use of any external tools. In particular, it does not require `pixi` to be installed.

## Project Discovery

Find files named `pixi.lock`. pixi also uses `pixi.toml` or `pyproject.toml` to declare dependencies, but only the presence of a lock file is used to detect pixi projects.

## Analysis

We parse `pixi.lock`, which is in the YAML format. The file has two halves that are joined on a package locator (the artifact URL, or a path/VCS spec):

- `environments` maps each environment name to each platform to the list of locators resolved for it.
- `packages` carries the metadata for each locator.

Conda packages are reported as conda dependencies named `'<channel>':<subdir>:<name>`, the same form the [conda](conda.md) strategy uses, so a package resolved by pixi and the same package resolved by conda are the same dependency in FOSSA. PyPI packages are reported as pip dependencies.

### Lock format versions

The `version:` field at the top of `pixi.lock` is checked against the versions this strategy understands:

| `version:` | Behaviour |
| ---------- | --------- |
| `5`, `6`, `7` | Analyzed. |
| anything else | Analysis fails with an error naming the version. It does not silently report an empty dependency graph. |

Version 5 spells out `name`, `version`, `build` and `subdir` on each package and locates it by `url` or `path`. Versions 6 and 7 replace that with a single `conda:`/`pypi:` key holding the locator, and record no `name` or `version` for conda packages. Version 7 adds a top-level `platforms` block, which carries no dependency information.

### Unreadable entries

A `packages` entry this strategy cannot read — an unrecognised ecosystem key from a newer pixi, a conda URL with no parseable filename, a PyPI entry with no name — costs only that one package. It is reported as a warning and the rest of the lockfile is still analyzed. A single unusual package must never turn into a project that reports nothing.

## Supported

| Package source        | Reported as                        |
| --------------------- | ---------------------------------- |
| conda packages        | `conda` dependency                 |
| PyPI wheels and sdists| `pip` dependency                   |
| `git+` PyPI sources   | `git` dependency, pinned to the locked commit |

## Limitations

- **All platforms are unioned.** `pixi.lock` pins a separate package set for every platform the workspace targets, and fossa-cli has no reliable notion of which platform you intend to build for. Every platform's packages are reported. Scanning on macOS still reports the `linux-64` conda packages. Conda dependency names embed the subdir, so per-platform builds of the same package remain distinct dependencies and are not double-counted; a `noarch` package listed under several platforms is reported once.

- **The graph is flat.** Every locked package is reported as a direct dependency with no edges. `pixi.lock` does record `depends` for conda packages and `requires_dist` for PyPI packages, and the direct set could be read from `pixi.toml`, but neither is used yet. The set of dependencies is complete; the relationships between them are not represented.

- **Environments are reported, not filtered.** The `default` environment maps to the production environment. `dev`/`development` and `test`/`testing` map to the development and testing environments; any other environment name is preserved verbatim so it can be filtered by name. A package present in several environments is reported once, carrying all of them.

- **Conda names come from the artifact filename.** Lock versions 6 and 7 do not record a `name` or `version` for conda packages — the only place those exist is the artifact URL, so they are parsed out of `<name>-<version>-<build>.conda`. This follows conda's filename convention and handles names containing hyphens, but it is a convention rather than a guarantee. Version 5 records `name` and `version` explicitly and those are used when present.

- **Local path dependencies are skipped.** pixi can resolve a PyPI dependency from a path in the workspace — written as `.` or `./…` under `pypi:` in versions 6 and 7, or as a `path:` key in version 5. These have no registry coordinates, so they are skipped with a warning naming the package rather than reported as a PyPI package that does not exist.

- **Labeled channels are kept whole.** A package from `conda-forge/label/broken` is named `'conda-forge/label/broken':<subdir>:<name>`, not `'broken':…`. This matches how the [conda](conda.md) strategy names the same package, so one package resolved by both strategies is one dependency in FOSSA.
