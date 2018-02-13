# Maven Support

Maven support in FOSSA CLI depends on the following tools existing in your environment:

- Java
- Maven
- `mvn:dependencyList` plugin (supported by the official Maven team; auto-installed by Maven if missing)

## Configuration

Add a `maven` module with the **relative** path to the `pom.xml` in your root directory.

```yaml
analyze:
  modules:
    - name: your-maven-project
      path: pom.xml
      type: maven
```

If you have an existing passing production build, you can run `fossa` from within the build environment and it should succeed.

Otherwise, you can run `fossa build` to execute the default build command `mvn clean install -DskipTests -Drat.skip=true -f $PATH_TO_POM`.

## Troubleshooting

FOSSA CLI runs and parses the output of the `mvn:dependencyList` plugin to generate dependency IDs.  If FOSSA fails, your build or the `mvn:dependencyList` plugin might be failing.

Run `mvn:dependencyList -f $PATH_TO_POM` and check the output to diagnose what went wrong.