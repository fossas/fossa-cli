# Haskell

## Support

Haskell support in FOSSA CLI depends on one of following tools existing in your
environment:

- cabal-install (aka `cabal`), version 2.0 or newer
- `stack`

cabal-install is recommended.

## Configuration

### Automatic 

Run `fossa init`, which detects:
 
- cabal projects, via `cabal.project` and `*.cabal`
- stack projects, via `stack.yaml`

Refer to [Discovery](#Discovery) for more information on the auto-configuration
logic.

### Manual

Add a module with `type: haskell`, and `target`+`path` set to the directory
containing the top-level project (usually the directory containing a
`cabal.project` or `stack.yaml`).

Additionally, configure the relevant strategy (either `cabal-install` or
`stack`). See example below.

```yaml
analyze:
  modules:
  - name: lens
    type: haskell
    target: .
    path: .
    options:
      strategy: cabal-install
```

## Options

| Option     | Type   | Name                         | Description                                                                              |
| ---------- | :----: | ---------------------------- | ---------------------------------------------------------------------------------------- |
| `strategy` | string | [Strategy](#strategy-string) | Used to specify the build tool used for this project (either `cabal-install` or `stack`) |


#### `strategy: <string>` 

Used to specify the build tool used for this project. Can be one of:

- `cabal-install`
- `stack`

## Discovery

Haskell discovery traverses the filetree, looking for `cabal.project`,
`stack.yaml`, and `*.cabal` files.

Because cabal and stack projects point to individual `*.cabal` files, we remove
`*.cabal` entries in a deduplication pass: `*.cabal` files with a project file
in the current or any parent directory won't be included in the final output.

## Analysis

Analysis differs for each of the build tools.

### cabal-install

Projects defined in `cabal.project` and standalone `*.cabal` packages are
analyzed the same way.

1. A solver plan (from `cabal new-build`) is generated if it doesn't already
exist via `cabal v2-build --dry-run`

2. The solver plan is analyzed and its dependency graph extracted (from `dist-newstyle/cache/plan.json`)

> note: analysis will fail if a solver plan doesn't exist and isn't able to be
generated. It's best to build the project before running `fossa analyze`

### stack

The stack analyzer builds out the dependency graph for a project using:

1. The list of all dependencies (both direct and transitive) via `stack ls dependencies`
2. The list of direct dependencies via `stack ls dependencies --depth 1`
3. The global dependency graph from ghc-pkg with `stack exec -- ghc-pkg dot`

## FAQ

### Q: Why isn't my stack project showing all of its dependencies?

Make sure to build your project first, then run `fossa analyze`. This ensures
the dependencies are available for analysis

### Q: Why are my projects appearing as dependencies of themselves?

This is a side effect of the way cabal (the library) structures
library+executable projects. You can safely ignore those references, and
they won't affect the output of analysis.
