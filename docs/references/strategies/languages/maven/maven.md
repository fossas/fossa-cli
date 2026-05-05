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

### I need to set some custom command line arguments for Maven, does FOSSA support that?

Not directly, but `mvn` itself has an [environment variable](https://maven.apache.org/configure.html) which you can use. 
For example, to set a custom `settings.xml` file you can use an invocation like this:

```sh
MAVEN_ARGS="--settings /foo/bar/settings.xml" fossa analyze
```

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

## Filtering by submodules

If you have maven project which has one or more subprojects, you may
only want to analyze a specific set of subprojects in some cases.


In `fossa-cli`, this can be achieved by using [exclusion filtering](./../../../files/fossa-yml.md).

1) Run `fossa list-targets`, to identify project directory and identifier of subprojects.
```bash
[ INFO] Found project: maven@./
[ INFO] Found target: maven@./:com.fossa:app
[ INFO] Found target: maven@./:com.fossa:list
[ INFO] Found target: maven@./:com.fossa:utilities
```

Note that, targets are denoted in following format `type@path:target`. For example `maven@./:com.fossa:utilities`:

Note: maven submodules targets are composed of `<groupId>:<artifactId>`, so the utilities submodule here is referenced by "com.fossa:utilities"
```
maven   @           ./      :      com.fossa:utilities
------ ---          ---    ---        -----------
Type   Path         Path   Target      Target
       separator           separator
```

2) Now to analyze only `utilities`, use a `.fossa.yml` file in the project root.

```yaml
# filename: .fossa.yml
#
# analyze only maven@./:com.fossa:utilities
version: 3
targets:
  only:
    - type: maven
      path: ./
      target: 'com.fossa:utilities'
```

Likewise, if you want to exclude specific set of subprojects, you can do following:

```yaml
# filename: .fossa.yml
#
# do not analyze maven@./:com.fossa:app, and maven@./:com.fossa:utilities
version: 3
targets:
  only:
    - type: maven
  exclude:
    - type: maven
      path: ./
      target: 'com.fossa:app'
    - type: gradle
      path: ./
      target: 'com.fossa:utilities'
```

1) Running `fossa analyze` will only analyze `list` submodule.

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
