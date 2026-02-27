# Bazel Analysis

Bazel projects using [Bzlmod](https://bazel.build/external/module) (`MODULE.bazel`) are analyzed for dependencies. This covers `bazel_dep()` entries (modules from the Bazel Central Registry) and Maven artifacts declared via the `rules_jvm_external` extension.

**Requires Bazel 6+** (Bzlmod available). The legacy `WORKSPACE` system is not supported. Bazel 9 (Jan 2026) removed `WORKSPACE` entirely, making `MODULE.bazel` the only dependency management approach.

Dependencies managed by other ecosystems (Go, Node, Cargo, pip, etc.) within a Bazel project are handled by their respective strategies.

| Strategy       | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| -------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| MODULE.bazel   | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |

## Project Discovery

Find a file named `MODULE.bazel`. Subdirectories are not scanned for nested Bazel projects, since Bazel projects are rooted at the `MODULE.bazel` location.

## Analysis

### Static analysis

1. Parse `MODULE.bazel` to extract:
   - `bazel_dep(name, version)` entries (reported as `bazel` dependency type)
   - `maven.install(artifacts=[...])` from `rules_jvm_external` extensions (reported as `mvn` dependency type)
   - Variable references are resolved (e.g. `ARTIFACTS = [...]` used in `maven.install(artifacts = ARTIFACTS)`)
2. If `maven_install.json` exists alongside `MODULE.bazel`, parse it for the full resolved Maven dependency tree with transitive edges.
3. If `maven_install.json` is absent but `maven.install(artifacts=[...])` is present in `MODULE.bazel`, report those artifacts as direct-only dependencies (graph breadth is `Partial`).

### Dynamic analysis

If `bazel` is available on `PATH`, run `bazel mod graph --output json` to obtain the full resolved Bazel module dependency tree with transitive edges.

If `bazel` is not available, analysis falls back to static-only. Use `--static-only-analysis` to skip the dynamic step explicitly.

## Supported files

| File                  | Required | Purpose                                        |
| --------------------- | -------- | ---------------------------------------------- |
| `MODULE.bazel`        | Yes      | Bazel module definition and dependency manifest |
| `maven_install.json`  | No       | Coursier-resolved Maven lockfile               |

## FAQ

### How do I only perform analysis for Bazel?

Specify the target in `.fossa.yml`:

```yaml
# .fossa.yml

version: 3
targets:
  only:
    - type: bazel
```

### What about WORKSPACE-based projects?

Only Bzlmod (`MODULE.bazel`) is supported. Bazel 9 removed `WORKSPACE` support entirely. For older projects, migrate to Bzlmod or use `WORKSPACE.bzlmod` as a transitional step.

### What about Maven artifacts declared in `rules_jvm_external`?

If you use `maven.install(artifacts=[...])` in `MODULE.bazel` and run `bazel run @maven//:pin` to generate `maven_install.json`, the lockfile will be parsed for the full transitive Maven dependency graph. Without the lockfile, only the directly declared artifact coordinates are reported.
