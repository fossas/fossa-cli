# Maven Support

Maven support in Fossa CLI depends on the following tools existing in your environment:

- Java
- Maven
- `mvn:dependencyList` plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Configuration

Add a `MavenArtifact` module with the **relative** path to the `pom.xml` in your root directory.

```yaml
analyze:
  modules:
    - name: yourmavenpackage
      path: pom.xml
      type: MavenArtifact
```

If you have an existing passing production build, you can run `fossa` in your existing environment and it should succeed.

Otherwise, you can run `fossa build` to execute with a default build command `mvn clean install -DskipTests -Drat.skip=true -f $PATH_TO_POM`.

## Troubleshooting

Fossa CLI runs and parses the output of the `mvn:dependencyList` plugin to generate dependency IDs.  If FOSSA fails, chances are your build or the `mvn:dependencyList` plugin is failing.

Run `mvn:dependencyList -f $PATH_TO_POM` and check the output to diagnose what went wrong.