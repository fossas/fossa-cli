# Maven pom.xml

Maven projects use `pom.xml` files to declare dependencies and multi-project relationships.

In a multi-project maven structure, sub-projects usually inherit dependencies, or can reference other sub-projects as dependencies.

## Project discovery

Do a complete directory traversal looking for files named `pom.xml`.

There's no guarantee that a maven pom will be called `pom.xml`. To accomodate for this, we use `<relativePath>` directives in `<parent>` declarations to include additional poms.

poms are linked together by their `<parent>` references into multi-project projects.

If the CLI encounters a maven pom file which references a parent, the package will still be considered a target for analysis if the parent doesn't also reference the child in its `modules` directory.
This is to fix confusing behavior where if a package and its parent were both found in the directory under analysis, the child would not be considered a target while its parent would be.

## Analysis

Each project in the single- or multi-project structure has its pom information overlayed on top of parent poms, and a dependency graph is gathered from each project.

We have limited support for naive `${property}` interpolation.
