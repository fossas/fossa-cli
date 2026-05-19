# TODO

## Goal
Refactor `PnpmLock.hs` so each lockfile version (v4/v5, v6/v7/v8, v9) has its own discrete graph builder, with version dispatch only at the top level of `buildGraph`.

## Tasks

### 1. Lift shared helpers out of `buildGraph` where-clause
- [x] Move `toDependency`, `toDep`, `withoutSymConstraint`, `withoutPeerDepSuffix`, `applyLabels`, `withoutLocalPackages`, `PnpmLabel` to top-level definitions.

### 2. Create version-specific helper functions at top level
- [x] Promote `getPkgNameVersionV5`, `getPkgNameVersionV6`, `getPkgNameVersionV9` to top-level.
- [x] Create `mkPkgKeyV5`, `mkPkgKeyV6`, `mkPkgKeyV9` as top-level functions.
- [x] Create `toResolvedDependency` as a top-level function parameterized by the package map, key-maker, and `toEnv` callback.

### 3. Factor shared graph-building loop
- [x] Extract the common loop body into `buildGraphCore` that takes callbacks for version-specific behavior (key parsing, key making, env resolution, label usage, snapshot edges).

### 4. Create per-version `buildGraph` entry points
- [x] `buildGraphV4or5` — `getPkgNameVersionV5`, `mkPkgKeyV5`, `toEnvInline`, no labels.
- [x] `buildGraphV678` — `getPkgNameVersionV6`, `mkPkgKeyV6`, `toEnvInline`, no labels.
- [x] `buildGraphV9` — `getPkgNameVersionV9`, `mkPkgKeyV9`, `toEnvEmpty`, labels enabled, snapshot edges.

### 5. Top-level dispatch in `buildGraph`
- [x] Simple `case lockFileVersion of` delegating to the three builders above.

### 6. Clean up and verify
- [x] Remove `isV9` local, `getPkgNameVersion` dispatcher, `mkPkgKey` dispatcher, `toEnv` version-check, and `when isV9` guards.
- [x] `cabal build` — clean, zero warnings.
- [x] `cabal test unit-tests` — 1374 examples, 0 failures.

### 7. Follow-up refinements
- [x] Replace `Bool useLabels` with `LabelingMode` sum type (`LabelingOff`/`LabelingOn`).
- [x] Share `getPkgNameVersionV6`/`V9` via common `parseAtKey :: Bool -> Text -> Maybe (Text, Text)`.
- [x] Extract `buildGraphPreV9` to eliminate duplicate `buildGraphV4or5`/`V678` bodies.
- [x] Fix context string "Npm Lockfile" → "Pnpm Lockfile".
- [x] Remove unused `Control.Monad (when)` import.
- [x] `cabal test unit-tests` — 1374 examples, 0 failures.

## Notes
- The `PnpmLockfile` data type and its `FromJSON` instance stay as-is — the refactor targets graph construction only.
- `ProjectMapDepMetadata` parser handles both v5/v6 formats via `Yaml.Value` pattern matching — acceptable as a leaf parser.
- `resolveCatalogVersion` was already top-level and shared — no change needed.
