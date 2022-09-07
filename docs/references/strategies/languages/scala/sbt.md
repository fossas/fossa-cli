# sbt Analysis

While the other analysis strategies for `gradle` and `maven` offer some scala project coverage, scala projects overwhelmingly use the build tool `sbt`.

| Strategy           | Direct Deps        | Deep Deps          | Edges              | Container Scanning (experimental) |
| ------------------ | ------------------ | ------------------ | ------------------ | --------------------------------- |
| sbt dependencyTree | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| pom                | :white_check_mark: | :x:                | :x:                | :x:                               |

> `sbt dependencyTree` Requires sbt version greater than v1.3.13 or `sbt-dependency-graph` plugin. Versions do not include classifiers. Does not support multi-project build.
 
# Requirements

- A locally-installed `sbt`
- Ensure project is compiled with `sbt compile` or equivalent

## Project Discovery

Directories that contain `build.sbt` files are treated as sbt projects

## Analysis

1. Run `sbt makePom` to generate pom files

## Strategy: `sbt dependencyTree`

1. From the generated pom file, identify the project name via the `<name>` attribute.
2. Perform `sbt $project/dependencyTree` and parse its output to create a dependency graph

With [sbt 1.4.0 release](https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced), the `dependencyTree` command is available by default. If you are using
an older sbt version, you can install the following plugin: https://github.com/sbt/sbt-dependency-graph. This will also enable fossa-cli to work with the `dependencyTree` task.

For sbt < 1.3:
```
# in project/plugins.sbt
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

> Unfortunately, the `sbt dependencyTree` command does not include version classifier in its output. Further, 
> currently we do not support this strategy for multi-project builds due an [inconsistent reporting defect in sbt](https://github.com/sbt/sbt/issues/6905).

## Strategy: `pom`

1. From generated pom.xml, use maven strategy to "link together" related poms into projects, and extract a dependency graph

## FAQ

### How do I *only* analyze scala projects?

You can explicitly specify analyses target in `.fossa.yml` file. 
Example below, will exclude all analyses targets except scala. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: scala
```
