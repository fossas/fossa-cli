# Maven Analysis

Maven projects are notoriously difficult to resolve into final dependency
graphs. While many dependencies are declared as XML, these dependency
declarations can span many buildscripts and user settings files. What's worse:
maven plugins are often used to apply dependencies to the project, and some
maven plugins allow arbitrary executable code -- similar to gradle.

To work around this, we're using the maven cli in conjunction with the [depgraph
maven plugin](https://github.com/ferstl/depgraph-maven-plugin), version 3.3.0.
This plugin is used by some Jenkins and Apache projects, so we can expect it to
be pretty solid.

| Strategy    | Direct Deps | Deep Deps | Edges | Tags |
| ---         | ---         | ---       | ---   | ---  |
| mavenplugin | ✅          | ✅        | ✅    |      |

## Project Discovery

find `pom.xml` files, and run `mvn validate` in those directories. When
`mvn validate` succeeds, skip all subdirectories. `mvn validate` ensure that we
have a valid maven project to analyze

## Analysis

1. unpack the embedded plugin to a temporary directory
2. install it to the local maven repository (`mvn install:install-file ...`)
3. invoke the plugin in the top-level project
