# sbt Analysis

While the other analysis strategies for `gradle` and `maven` offer some scala project coverage, scala projects overwhelmingly use the build tool `sbt`.

| Tactics                     | Direct Deps        | Transitive Deps    | Edges              | Container Scanning (experimental) |
| ---------------------------- | ------------------ | ------------------ | ------------------ | --------------------------------- |
| sbt dependencyBrowseTreeHTML | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| sbt dependencyTree           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| pom                          | :white_check_mark: | :x:                | :x:                | :x:                               |

> `sbt dependencyBrowseTreeHTML` and `sbt dependencyTree` requires `sbt-dependency-graph` plugin.
 
# Requirements

- A locally-installed `sbt`
- Ensure project is compiled with `sbt compile` or equivalent

## Project Discovery

Directories that contain `build.sbt` files are treated as sbt projects

## Analysis

1. Run `sbt makePom` to generate pom files

## Tactic: `sbt dependencyTree`

1. From the generated pom file, identify the project name via the `<name>` attribute.
2. Perform `sbt $project/dependencyTree` and parse its output to create a dependency graph

With [sbt 1.4.0 release](https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced), 
the `dependencyTree` command is available by default. If you are using an older sbt version, 
you can install the following plugin: https://github.com/sbt/sbt-dependency-graph. 
This will also enable fossa-cli to work with the `dependencyTree` task.

For this tactic, you will need to ensure `dependencyTree` is accessible. To do so, 
please use [sbt-dependency-graph](https://github.com/sbt/sbt-dependency-graph) plugin.

For `sbt >= 1.4`,
```
# in project/plugins.sbt
addDependencyTreePlugin
```

For `sbt < 1.3`,
```
# in project/plugins.sbt
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

> This tactic does not support multi-project builds due an [inconsistent reporting defect in sbt](https://github.com/sbt/sbt/issues/6905)
> This `sbt dependencyTree` command does not include version classifier (if any) in it's output and consequently, version classifiers are ignored.

## Tactic: `sbt dependencyBrowseTreeHTML`

1. Run `sbt dependencyBrowseTreeHTML`
2. From the generated `tree.html` file, find corresponding `tree.json` file
3. Find corresponding pom file generated via (step 1 of analysis)
4. Parse `tree.json` to create a dependency graph

For this tactic, you will need to ensure `dependencyBrowseTreeHTML` is accessible. To do so, 
please use [sbt-dependency-graph](https://github.com/sbt/sbt-dependency-graph) plugin.

For `sbt >= 1.4`,
```
# in project/plugins.sbt
addDependencyTreePlugin
```

For `sbt < 1.3`,
```
# in project/plugins.sbt
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

> This tactic supports multi-project builds.
> This `sbt dependencyBrowseTreeHTML` command does not include version classifier (if any) in it's output and consequently, version classifiers are ignored.

## Tactic: `pom`

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

### Why does `fossa-cli` use `sbt dependencyBrowseTreeHTML` command?

`dependencyBrowseTreeHTML` command generates dependency graph in JSON
format per each project in sbt build. Unlike `dependencyBrowseTree` command, 
it does not open a browser, once command is successfully ran. It produces graph
in format that is easy to parse and diagnose.