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

### Packages

Package keys use a slash-delimited path for nested `node_modules`:

- `"lodash"` — top-level package
- `"cross-spawn/which"` — `which` nested under `cross-spawn`

Package values are variable-length arrays depending on the resolution type:

- **npm:** `["name@version", "registry", {deps}, "integrity"]`
- **file:** `["name@file:path", {deps}]`
- **workspace:** `["name@workspace:path"]`
- **git:** `["name@git+url", {deps}, "hash", "integrity"]`

### Environment Labeling

- Dependencies declared in `devDependencies` of any workspace are labeled as development dependencies.
- Dependencies declared in `dependencies` or `optionalDependencies` of any workspace are labeled as production dependencies.
- Workspace packages themselves (those with `workspace:` resolutions) are excluded from the final dependency graph.
