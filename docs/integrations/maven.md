# Maven

## Support

Maven support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Maven (defaults to `mvn`, configure with `$MAVEN_BINARY`, or the `bin` option)
- The Maven Dependency Plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Configuration

Automatic: Run `fossa init` to detect all directories with a `pom.xml` file. The CLI will follow references to
other Maven modules (or projects) listed under `<modules>` in the POM manifest. A FOSSA CLI module will be
created for each Maven project.

Manual: Add a `mvn` module with either
1. `path` set to "." and `target` to either the relative path to the Maven project's POM file or to the Maven
   project's directory which has a "pom.xml" file; or
2. `path` set to the directory of the Maven project's directory and `target` to the ID of the project.

```yaml
analyze:
  modules:
    - name: My mvn Project
      type: mvn
      path: .
      target: pom.xml
```

## Options
| Option     |  Type  | Name                        | Common Use Case                                      |
| ---------- | :----: | --------------------------- | ---------------------------------------------------- |
| `bin`      | string | [Binary](#bin-string)       | Path to the Maven executable.                        |
| `strategy` | string | [Command](#strategy-string) | The dependency analysis strategy.                    |
| `cmd`      | string | [Command](#cmd-string)      | Custom command that outputs a Maven dependency tree. |

### `bin: <string>`

Specify the executable for the CLI to use to run commands. By default, the CLI will first check if the
environment variable `$MAVEN_BINARY` is set and use it if is, and otherwise it will use the `mvn` binary that
is found.

### `strategy: <string>`

Specify the strategy for dependency resolution. Setting to "pom-file" tells the CLI to only look at the POM
file for the module. Setting to "maven-tree" tells the CLI to only run Maven to get a dependency tree. By
default, the CLI will use "maven-tree", and if an error occurs or no dependencies are found then the CLI falls
back to the "pom-file" strategy.

### `cmd: <string>`

If `strategy` is not set, then `cmd` can specify the command for the CLI to use to retrieve the dependency
tree from Maven in the format that running the `dependency:tree` goal outputs.

## Analysis

Fossa analyzes a Maven project by parsing the output from `mvn dependency:tree` into a dependency graph.
