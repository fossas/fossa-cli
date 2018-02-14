# Maven

## Installation

Maven support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Maven (defaults to `mvn`, configure with `$MAVEN_BINARY`)
- `mvn:dependencyList` plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Usage

Add a `mvn` module with the path to the `pom.xml` in your project.

```yaml
analyze:
  modules:
    - name: your-mvn-project
      path: pom.xml
      type: mvn
```

## Design
### Building

Builds are run using `mvn install -DskipTests -Drat.skip=true`. If `--force` is set, the build command also runs `mvn clean` before running the build.

### Analysis

Analysis parses the output of `mvn dependency:list`.
