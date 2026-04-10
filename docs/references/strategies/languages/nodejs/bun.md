# Bun

[Bun](https://bun.sh/) is a fast JavaScript runtime and package manager.
Bun uses a `bun.lock` lockfile in JSONC format (JSON with Comments).

Reference: https://bun.sh/docs/install/lockfile

## Project Discovery

Find files named `bun.lock` with a corresponding `package.json` file.

## Analysis

Only `bun.lock` is used for analysis. The lockfile is in JSONC format,
meaning it may contain single-line comments (`//`), block comments (`/* */`),
and trailing commas. These are stripped before parsing.

### Lockfile Structure

The `bun.lock` file has the following top-level structure:

```jsonc
{
  "lockfileVersion": 1,
  "workspaces": {
    "": { "name": "my-project", "dependencies": {...}, "devDependencies": {...} },
    "packages/a": { "name": "pkg-a", "dependencies": {...} }
  },
  "packages": {
    "lodash": ["lodash@4.17.21", "", {}, "sha512-..."],
    "cross-spawn/which": ["which@2.0.2", "", {...}, "sha512-..."]
  }
}
```

### Workspaces

Workspace entries are keyed by their relative path from the root, with `""`
representing the root workspace. Each workspace declares its own
`dependencies`, `devDependencies`, and `optionalDependencies`.

Each workspace package (including the root) is exposed as an individual build
target (e.g. `bun@./:my-app`, `bun@./:lib-utils`). When a subset of targets
is selected via `.fossa.yml`, only those packages' dependencies are included
in the analysis. When no filtering is applied, all targets are selected and
all dependencies from every workspace package are included.

### Packages

Package keys use a slash-delimited path for nested `node_modules`:

- `"lodash"` — top-level package
- `"cross-spawn/which"` — `which` nested under `cross-spawn`

Package values are variable-length arrays depending on the resolution type:

- **npm:** `["name@version", "registry", {deps}, "integrity"]`
- **git:** `["name@git+url", {deps}, "hash", "integrity"]`
- **github:** `["name@github:user/repo#ref", {deps}, "resolved"]`
- **file:** `["name@file:path", {deps}]`
- **link:** `["name@link:path", {deps}]`
- **workspace:** `["name@workspace:path"]`

Only **npm** and **git/github** packages are included in the dependency graph.
File, link, workspace, root, and module resolutions are excluded.

### Environment Labeling

- Dependencies declared in `devDependencies` of any workspace are labeled as development dependencies.
- Dependencies declared in `dependencies` or `optionalDependencies` of any workspace are labeled as production dependencies.
- When the same package appears in both `dependencies` and `devDependencies` across
  different workspaces, both environments are recorded on a single graph vertex
  (environments accumulate rather than creating duplicate entries).
