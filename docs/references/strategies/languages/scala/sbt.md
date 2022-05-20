# sbt Analysis

While the other analysis strategies for `gradle` and `maven` offer some scala project coverage, scala projects overwhelmingly use the build tool `sbt`.

| Strategy           | Direct Deps        | Deep Deps          | Edges              | Other Limitations                                                 |
| ------------------ | ------------------ | ------------------ | ------------------ | ----------------------------------------------------------------- |
| sbt dependencyTree | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | Requires sbt v1.4 or greater. Versions do not include classifiers |
| pom                | :heavy_check_mark: | :x:                | :x:                |                                                                   |

 # Requirements

- A locally-installed `sbt`

## Project Discovery

Directories that contain `build.sbt` files are treated as sbt projects

## Analysis

1. Run `sbt makePom` to generate pom files

## Strategy: `sbt dependencyTree`

1. From generated pom file, identify project name via, `<name>` attribute.
2. Perform `sbt $project/dependencyTree` and parse it's output to create dependency graph

With [sbt 1.4.0 release](https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced), `dependencyTree` command is available by default. If you are using
older sbt version, you can install following plugin: https://github.com/sbt/sbt-dependency-graph, which will also enable fossa-cli to work with `dependencyTree` task.

For sbt < 1.3:
```
# in project/plugins.sbt
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

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
