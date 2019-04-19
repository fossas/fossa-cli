# Haskell

## Support

Haskell support in FOSSA CLI depends on one of following tools existing in your environment:

- cabal-install (version 2.0 or newer)
- stack

## Configuration

### Automatic 

Run `fossa init` which detects all cabal (via `cabal.project` or `*.cabal` files) and stack projects. Refer to [Discovery](#Discovery) for more information on the auto-configuration logic.

### Manual

Add a module with `type: haskell`, and `target`+`path` set to the directory containing the top-level project (usually the directory containing a`cabal.project` or `stack.yaml`). Additionally, configure a strategy (either `cabal-install` or `stack`). See example below.

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

| Option                         | Type   | Name                         | Description                                                                              |
| ------------------------------ | :----: | ---------------------------- | ---------------------------------------------------------------------------------------- |
| `strategy`                     | string | [Strategy](#strategy-string) | Used to specify the build tool used for this project (either `cabal-install` or `stack`) |


#### `strategy: <string>` 

Used to specify the build tool used for this project. Can be one of:

- `cabal-install`
- `stack`

## Analysis

Analysis differs based on the build tool

### cabal-install

Projects defined in cabal.project and standalone cabal packages are analyzed the same way.

1. A solver plan (from `cabal new-build`) is generated if it doesn't already exist via `cabal new-build --dry-run`
2. The solver plan is analyzed and its dependency graph extracted (from `dist-newstyle/cache/plan.json`)

> note: analysis will fail if a solver plan doesn't exist and isn't able to be generated. Ensure that `cabal new-build` has been or can be run from the project directory

### stack

The stack analyzer builds out the dependency graph for a project using:

1. The list of all dependencies (both direct and transitive) via `stack ls dependencies`
2. The list of direct dependencies via `stack ls dependencies --depth 1`
3. The global dependency graph from ghc-pkg with `stack exec -- ghc-pkg dot`

## Known limitations

- The stack analyzer won't hydrate the edges of the dependency graph unless dependencies have been installed -- usually via `stack build` or similar
