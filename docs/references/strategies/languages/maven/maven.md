# Maven Analysis

For maven projects, we offer a more-accurate strategy (mavenplugin), and a strategy with zero requirements (pomxml).

| Strategy                      | Direct Deps | Deep Deps | Edges |
| ----------------------------- | ----------- | --------- | ----- |
| [mavenplugin](mavenplugin.md) | ✅           | ✅         | ✅     |
| [treecmd](treecmd.md)         | ✅           | ✅         | ✅     |
| [pomxml](pomxml.md)           | ✅           | ❌         | ❌     |

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
