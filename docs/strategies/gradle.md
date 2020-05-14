# Gradle Analysis

When using gradle, users specify dependencies and plugins with a groovy DSL.
This makes accurate dependency analysis particularly difficult: groovy projects
tend to contain several (sub)projects with their own buildscripts, and plugins
that add dependencies of their own.

To accurately determine the entire project/dependency structure, we use an
external script.

| Strategy   | Direct Deps | Deep Deps | Edges | Tags |
| ---        | ---         | ---       | ---   | ---  |
| initscript | ✅          | ✅        | ✅    |      |

## Project Discovery

find `build.gradle*` files, and run `gradle tasks` in those directories. When
`gradle tasks` succeeds, skip all subdirectories. `gradle tasks` ensures that we
have a valid gradle project to analyze.

## Analysis

1. Unpack our init script to a temporary directory
2. Invoke the init script with `gradle jsonDeps -Ipath/to/init.gradle`

Currently, we only record the `default` configuration dependencies of each
project. Subprojects are also included in the graph.
