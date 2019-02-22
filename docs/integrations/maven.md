# Maven

## Support

Maven support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Maven (defaults to `mvn`, configure with `$MAVEN_BINARY`, or the Binary option)
- `mvn:dependencyList` plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Configuration

Automatic: Run `fossa init` to detect all directories with a `pom.xml` file at their root. fossa parses the `pom.xml` file to create a fossa module for each module within the maven project.

Manual: Add a `mvn` module with `path` set to the root of a maven project and `target` to the maven module desired to be analyzed.

```yaml
analyze:
  modules:
    - name: your-mvn-project
      type: mvn
      path: <module>
      target: .
```

## Options
| Option |  Type  | Name                          | Common Use Case                                      |
| ------ | :----: | ----------------------------- | ---------------------------------------------------- |
| `bin`  | string | [Binary](#Binary:-<string>)   | Custom maven binary executable.                      |
| `cmd`  | string | [Command](#Command:-<string>) | Custom command that outputs a maven dependency tree. |

## Analysis

Fossa analyzes a maven project by parsing the output from `maven dependency:tree --batch-mode --projects <project>` into a dependency graph.
