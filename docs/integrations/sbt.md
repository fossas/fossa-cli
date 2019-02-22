# SBT

## Support

SBT support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- SBT (defaults to `sbt`, configure with `$SBT_BINARY`)

In addition, SBT requires the following plugin to be installed:
- `net.virtual-void.sbt-dependency-graph`: install by adding `addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")` to your SBT configuration

## Configuration

Automatic: Run `fossa init` to detect all directories with a `build.sbt` file at their root. fossa runs `sbt projects` at these directories roots and creates a module for each sbt project with configuration set to `compile` by default.

Manual: Add an `sbt` module with the path to the `build.sbt` file in your project. Set target to `<project>:<configuration>` where project is the desired sbt project and configuration is the configuration analysis is desired for.

```yaml
analyze:
  modules:
    - name: your-sbt-project
      type: sbt
      path: build.sbt
      target: <project>:<configuration>
```

## Analysis

Analysis parses the output of `sbt -no-colors <project>/<configuration>:dependencyList` to retrieve a full list of dependencies utilized by your project.

## Known Limitations

- Currently fossa can only analyze a project if the user has `net.virtual-void.sbt-dependency-graph` installed. 