# GHC 9.10 CI Upgrade — Handoff Document

## Context

The `ghc-9.10-upgrade` branch (PR: <https://github.com/fossas/fossa-cli/pull/new/ghc-9.10-upgrade>) updates the project to build with GHC 9.10.3. The Haskell code changes are complete and verified. The remaining work is building and integrating a new CI docker image.

## PR Link (fused-effects-exceptions)

<https://github.com/fused-effects/fused-effects-exceptions/pull/22>

This PR (opened 2024-08-15) relaxes the `transformers` bound from `<0.6` to allow `0.6`. It is still open and unmerged. Once merged and released, we can remove the `allow-newer` override for `fused-effects-exceptions:transformers` in `cabal.project.common`.

## Current CI Image State

**Registry:** `fossa/haskell-static-alpine` on Docker Hub  
**Description:** Statically linked distributions of GHC for building binaries in Docker  
**Repo:** <https://github.com/fossas/haskell-static-alpine>

**Existing tags:**

| Tag | Last Pushed | Architecture |
|-----|-------------|--------------|
| `ghc-9.8.4` | 2025-10-23 | amd64, arm64 |
| `ghc-9.8.2` | 2025-10-23 | amd64, arm64 |
| `ghc-9.4.8` | 2025-10-23 | amd64, arm64 |
| `ghc-9.4.7` | 2025-10-23 | amd64 |
| `ghc-9.0.2` | 2025-10-23 | amd64 |
| `ghc-8.10.7` | 2025-10-23 | amd64 |
| `ghc-8.8.4` | 2025-10-23 | amd64, arm64 |

**No `ghc-9.10.3` tag exists yet.**

## Required Changes

### 1. Build new docker image

Create `fossa/haskell-static-alpine:ghc-9.10.3` following the same pattern as existing images in the `fossas/haskell-static-alpine` repo.

Key requirements:

- Static binary build (like existing images)
- Support both `amd64` and `arm64` architectures
- Include GHC 9.10.3, cabal-install, and all build dependencies used by the project
- The image is used in GitHub Actions via `container: fossa/haskell-static-alpine:ghc-9.10.3`

### 2. Update CI workflows

**Files to modify:**

- `.github/workflows/build-all.yml`
- `.github/workflows/integrations-test.yml`
- `.github/workflows/bench.yml`

**Changes:**

- Update `container:` references from `ghc-9.8.4` to `ghc-9.10.3`
- Update matrix `ghc:` values from `'9.8.4'` to `'9.10.3'`
- Update version check string: `EXPECTED="fossa-cli version ... compiled with ghc-9.10"`

### 3. Remove allow-newer overrides (post-merge)

Once fused-effects-exceptions PR #22 is merged and released, remove from `cabal.project.common`:

```
, fused-effects-exceptions:transformers
```

## Verification Steps

After image is built and workflows updated:

```bash
# Build in the new image
docker run --rm -v $(pwd):/workspace fossa/haskell-static-alpine:ghc-9.10.3 \
  bash -c "cd /workspace && cabal build all --allow-newer --ghc-options=-Werror"

# Run tests
docker run --rm -v $(pwd):/workspace fossa/haskell-static-alpine:ghc-9.10.3 \
  bash -c "cd /workspace && cabal test unit-tests --allow-newer"

# Verify fossa runs
docker run --rm -v $(pwd):/workspace fossa/haskell-static-alpine:ghc-9.10.3 \
  bash -c "/workspace/dist-newstyle/build/x86_64-linux/ghc-9.10.3/spectrometer-0.1.0.0/x/fossa/noopt/build/fossa/fossa --version"
```

## Dependencies Status

| Dependency | Status | Action |
|------------|--------|--------|
| path-0.9.6 | ✅ Fixed | Fork at `fossas/path` commit `7da33f1` |
| codec-rpm | ✅ Fixed | Fork at `fossas/codec-rpm` commit `14b1262` |
| fused-effects-exceptions | ⏳ PR #22 open | Wait for merge, then remove allow-newer |
| semver | 🔶 Fork needed | hackage 0.4.0.1 doesn't expose `Data.SemVer.Internal` |
| CI docker image | 🔲 Not started | Build `ghc-9.10.3` tag |

## Related PRs/Issues

- **fused-effects-exceptions PR #22:** <https://github.com/fused-effects/fused-effects-exceptions/pull/22>
- **fused-effects-exceptions Issue #21:** <https://github.com/fused-effects/fused-effects-exceptions/issues/21>
- **lzma-conduit Issue #27:** <https://github.com/alphaHeavy/lzma-conduit/issues/27> (moribund package, may need vendor)
