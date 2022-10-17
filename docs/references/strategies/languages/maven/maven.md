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
