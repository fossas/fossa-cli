# Maven pom.xml

Maven projects use `pom.xml` files to declare dependencies and multi-project relationships.

In a multi-project maven structure, sub-projects usually inherit dependencies, or can reference other sub-projects as dependencies.

## Project discovery

Do a complete directory traversal looking for files named `pom.xml`.

There's no guarantee that a maven pom will be called `pom.xml`. To accomodate for this, we use `<relativePath>` directives in `<parent>` declarations to include additional poms.

poms are linked together by their `<parent>` references into multi-project projects.

## Analysis

Each project in the single- or multi-project structure has its pom information overlayed on top of parent poms, and a dependency graph is gathered from each project.

The root pom and every submodule are excluded from the reported graph, and the
dependencies they declare are reported as direct. This is the same treatment the
`mavenplugin` and `treecmd` tactics give them: those are the user's own projects,
not things they depend on. A pom file only declares direct dependencies, so this
tactic reports no transitive dependencies and no edges.

For example, given a project `com.example:root` whose `mod-a` submodule declares
`junit:junit` and whose `mod-b` submodule declares `com.google.guava:guava`, the
reported dependencies are `junit:junit` and `com.google.guava:guava`, both direct.
Neither `com.example:root` nor either submodule is reported.

We have limited support for naive `${property}` interpolation.
