# sbt Analysis

While the other analysis strategies for `gradle` and `maven` offer some scala project coverage, scala projects overwhelmingly use the build tool `sbt`.

| Tactics                        | Direct Deps        | Transitive Deps    | Edges              | Container Scanning (experimental) |
| ------------------------------ | ------------------ | ------------------ | ------------------ | --------------------------------- |
| `sbt dependencyBrowseTreeHTML` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| `sbt dependencyTree`           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| `sbt makePom`                  | :white_check_mark: | :x:                | :x:                | :x:                               |
 
# Requirements

- `sbt` executable in PATH
- Project can be compiled with `sbt compile` or equivalent
- [sbt-dependency-graph](#how-do-i-use-sbt-dependency-graph-plugin) plugin is accessible

## Project Discovery

Directories that contain `build.sbt` files are treated as sbt projects.

## Analysis

1. Run `sbt makePom` to generate pom files

## `sbt dependencyTree`

1. From the generated pom file, identify the project name via the `<name>` attribute.
2. Perform `sbt $project/dependencyTree` and parse its output to create a dependency graph

For this approach, you will need to ensure `dependencyTree` task is accessible. To do so, 
please use [sbt-dependency-graph](#how-do-i-use-sbt-dependency-graph-plugin) plugin.

### Limitations

- This tactic does not support multi-project builds due an [inconsistent reporting defect in sbt](https://github.com/sbt/sbt/issues/6905)
- This tactic requires that project is using [sbt-dependency-graph](#how-do-i-use-sbt-dependency-graph-plugin) plugin.
- Since `sbt dependencyTree` command's output does not include a version classifier, consequently, version classifiers are ignored.

## `sbt dependencyBrowseTreeHTML`

1. Run `sbt dependencyBrowseTreeHTML`
2. From the generated `tree.html` file, find the corresponding `tree.json` file
3. Find the corresponding pom file generated via (step 1 of the analysis)
4. Parse `tree.json` to create a dependency graph

For this approach, you will need to ensure the `dependencyBrowseTreeHTML` task is accessible. To do so, 
please use [sbt-dependency-graph](#how-do-i-use-sbt-dependency-graph-plugin) plugin.

### Limitations

- This tactic requires that project is using [sbt-dependency-graph](#how-do-i-use-sbt-dependency-graph-plugin) plugin.
- Since the `sbt dependencyBrowseTreeHTML` command's output does not include a version classifier, consequently, version classifiers are ignored.

## `sbt makePom`

1. From generated pom.xml, use the maven strategy to "link together" related poms into projects and extract a dependency graph

## FAQ

### How do I *only* analyze scala projects?

You can explicitly specify analyses target in `.fossa.yml` file. 
The example below will exclude all analysis targets except scala. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: scala
```

### How do I use [sbt-dependency-graph](https://github.com/sbt/sbt-dependency-graph) plugin?

To identify deep dependencies and edges among them, FOSSA CLI requires use of the following tasks:

- `dependencyBrowseTreeHTML` (for multi-project builds)
- `dependencyTree`

To add the [sbt-dependency-graph](https://github.com/sbt/sbt-dependency-graph) plugin only to a single project, 
put this line into `project/plugins.sbt` of your project:

With `sbt 1.4.0+`:

```
addDependencyTreePlugin
```
Refer to sbt: https://www.scala-sbt.org/1.x/docs/sbt-1.4-Release-Notes.html#sbt-dependency-graph+is+in-sourced


With `sbt < 1.4`:

```
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
```

Plugins can be installed for all your projects at once by declaring them in `$HOME/.sbt/$VERSION/plugins/`.
`$HOME/.sbt/$VERSION/plugins/` is an sbt project whose classpath is exported to all sbt build definition projects.

### Why does `fossa-cli` use `sbt dependencyBrowseTreeHTML` command?

`dependencyBrowseTreeHTML` command generates a dependency graph in JSON
format per each project in sbt build. Unlike `dependencyBrowseTree` command, 
it does not open a browser once command is successfully ran. It produces a graph
in a format that is easy to parse and diagnose.

## Does FOSSA CLI include standard library as a dependency?

Yes