# Maven Analysis

For maven projects, we offer a more-accurate strategy (mavenplugin), and a strategy with zero requirements (pomxml).

| Strategy                      | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| ----------------------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| [mavenplugin](mavenplugin.md) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |
| [treecmd](treecmd.md)         | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |
| [pomxml](pomxml.md)           | :white_check_mark: | :x:                | :x:                | :white_check_mark: |

Maven analysis attempts these analysis methods in order:
1. Run the maven plugin command version 4.0.1.
2. Run the maven tree command.
3. Run the maven plugin command version 3.3.0.
4. Scan `pom.xml` files located in the file tree.

FOSSA CLI uses [strategy command selection](../../../../features/strategy-command-selection.md) for Maven commands:
- Maven _candidate commands_ are:
  - If present, the command specified by the `FOSSA_MAVEN_CMD` environment variable.
  - If present, the `mvnw` or `mvnw.bat` script in the project directory or any ancestor directory.
  - Finally, the `mvn` command, which is searched for in `$PATH`.
- To choose a command from the candidates, FOSSA CLI runs each candidate with `-v` and selects the first one that succeeds.

## FAQ

### One of my transitive dependencies does not have path information
The mavenplugin and treecmd tactic can result in transitive dependencies which do not display paths to parents. This example graph shows how that can happen:
```
+- com.amazonaws:aws-java-sdk-kms:1.11.415:compile
|  +- com.amazonaws:aws-java-sdk-core:1.11.415:compile
\- com.jayway.restassured:rest-assured:2.9.0:test
   +- org.apache.httpcomponents:httpclient:4.5.1:compile   👈
```
`httpclient` will appear as a transitive dependency in the FOSSA UI, but it will not have any paths. There are a few things that contribute to this happening. `httpclient`'s only listed parent is `restassured` which is a `test` dependency, however, `httpclient` is a `compile`. This tells us that `httpclient` has another parent in the graph, but we are unable to determine where.

## Filtering by Maven Dependency Scope 

You can use [configuration file](../../../files/fossa-yml.md) to provide maven dependency scopes that you would like to filter. You can filter by either inclusion or exclusion. When both scope-only and scope-exclude are provided, scope-only will take precedence and be used for filtering.

```yaml
version: 3

maven:
  scope-only:
    - compile
    - runtime
## OR
  scope-exclude:
    - provided
    - system
    - test
```

<!--

TODO: write docs, like Gradle's.

Docs outline:

- Concepts
  - Multi-module reactor builds
  - POMs and POM closures
  - settings.xml
- Discovery
  - Finding pom.xmls
- Tactics
  - dependency:tree
  - POM parsing
 -->
