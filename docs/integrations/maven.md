# Maven

## Support

Maven support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Maven (defaults to `mvn`, configure with `$MAVEN_BINARY`, or the `bin` option)
- The Maven Dependency Plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Configuration

Automatic: Run `fossa init` to detect all directories with a `pom.xml` file at their root. The CLI will follow
references to other Maven modules (or projects) listed under `<modules>` in the POM manifest. A FOSSA CLI
module will be created for each Maven project.

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
| Option |  Type  | Name                          | Common Use Case                                      |
| ------ | :----: | ----------------------------- | ---------------------------------------------------- |
| `bin`  | string | [Binary](#Binary:-<string>)   | Custom Maven executable.                             |
| `cmd`  | string | [Command](#Command:-<string>) | Custom command that outputs a Maven dependency tree. |

## Analysis

Fossa analyzes a Maven project by parsing the output from `maven dependency:tree` into a dependency graph.
