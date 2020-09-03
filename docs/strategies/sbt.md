# sbt Analysis

While the other analysis strategies for `gradle` and `maven` offer some scala project coverage, scala projects overwhelmingly use the build tool `sbt`.

| Strategy   | Direct Deps | Deep Deps | Edges | Tags |
| ---        | ---         | ---       | ---   | ---  |
| sbt        | ✅          | ✅        | ✅    |      |

## Requirements

- A locally-installed `sbt`

## Project Discovery

Directories that contain `build.sbt` files are treated as sbt projects

## Analysis

1. Run `sbt makePom` to generate pom files
2. Use the pom.xml maven strategy to "link together" related poms into projects, and extract a dependency graph
