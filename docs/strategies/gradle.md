# Gradle Analysis

When using gradle, users specify dependencies and plugins with a groovy DSL. This makes accurate dependency analysis particularly difficult: gradle projects tend to contain several (sub)projects with their own buildscripts, and plugins that add dependencies of their own.

To accurately determine the entire project/dependency structure, we use an
initscript to inject our own `jsonDeps` task.

| Strategy   | Direct Deps | Deep Deps | Edges | Tags |
| ---        | ---         | ---       | ---   | ---  |
| initscript | ✅          | ✅        | ✅    |      |

## Requirements

Gradle project analysis requires at least one of:

- A project that uses gradle wrappers (`gradlew`/`gradlew.bat`)
- A locally-installed gradle

## Project Discovery

Directories that contain a gradle buildscript, e.g., `build.gradle` or `build.gradle.kts`, are treated as gradle projects. Gradle buildscripts in a project's subdirectories are ignored.

## Analysis

1. Unpack our init script to a temporary directory
2. Invoke the init script with `gradle jsonDeps -Ipath/to/init.gradle`

Currently, we only record the `default` configuration dependencies of each
project. Subprojects are also included in the graph.
