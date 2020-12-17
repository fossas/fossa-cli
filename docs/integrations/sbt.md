# SBT

## Support

SBT support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- SBT (defaults to `sbt`, configure with `$SBT_BINARY`)

In addition, SBT requires the following plugin to be installed:
- `net.virtual-void.sbt-dependency-graph`: install by adding `addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")` to your SBT configuration

## Configuration

Automatic: Run `fossa init` to detect all directories with a `build.sbt` file at their root. fossa runs `sbt projects` at these directories roots and creates a module for each sbt project with configuration set to `compile` by default.

Manual: Add an `sbt` module with the path to the directory where a `build.sbt` file is located in your project. Set target to `<project>:<configuration>` where project is the desired sbt project and configuration is the configuration analysis is desired for.

```yaml
analyze:
  modules:
    - name: your-sbt-project
      type: sbt
      path: .
      target: <project>:<configuration>
```

## Analysis

Analysis parses the output of `sbt -no-colors <project>/<configuration>:dependencyList` to retrieve a full list of dependencies utilized by your project.

## Known Limitations

- Currently fossa can only analyze a project if the user has `net.virtual-void.sbt-dependency-graph` installed. 
- To use SBT 1.3.x you will need to add `ThisBuild / useCoursier := false` to your `build.sbt`, because coursier was [made the default](https://www.scala-sbt.org/1.x/docs/sbt-1.3-Release-Notes.html#Library+management+with+Coursier) and [does not pull license info](https://github.com/coursier/coursier/issues/1790).
- You cannot use SBT 1.4.x presently as the `net.virtual-void.sbt-dependency-graph` plugin has been [insourced](https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced).
- For initialisation to work robustly you may need to disable any configuration which writes output `onLoad`
