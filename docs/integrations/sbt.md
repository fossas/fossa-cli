# SBT

## Installation

SBT support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- SBT (defaults to `sbt`, configure with `$SBT_BINARY`)

In addition, SBT must have the following plugin installed:
- `net.virtual-void.sbt-dependency-graph`: install by adding `addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")` to your SBT configuration

## Usage

Add a `sbt` module with the path to the `build.sbt` in your project.

```yaml
analyze:
  modules:
    - name: your-sbt-project
      path: build.sbt
      type: sbt
```

## Design
### Building

Builds are run using `sbt compile`. If `--force` is set, the build command also runs `sbt clean` before running the build.

### Analysis

Analysis parses the output of `sbt -no-colors dependencyList`.
