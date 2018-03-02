# Gradle

## Installation

Gradle support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Gradle (defaults to `gradle` or local `gradlew`, configure with `$GRADLE_BINARY`)

## Usage

### Automatic Configuration

When running `fossa init`, FOSSA will look for a root Gradle build (`build.gradle`) and interrogate it for build-able tasks.  From there, it will write module configurations into your `.fossa.yml` for every task that supports a `:dependency` sub-task.

If a gradle wrapper was provided in the same directory (`./gradlew`), `fossa` will prefer to use that over the local `gradle` binary.

### Manual Configuration

Add a `gradle` module with the path to the `pom.xml` in your project.

```yaml
analyze:
  modules:
    - name: your-mvn-project
      path: pom.xml
      type: mvn
```

## Design

### Analysis

Analysis will support any tasks you can run `:dependendencies` on with the "compile" configuration.  This is equivalent to `gradle app:dependencies --configuration=compile`.
