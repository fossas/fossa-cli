# Maven plugin

| Strategy           | Direct Deps | Transitive Deps | Edges | Tags       |
| ---                | ---         | ---       | ---   | ---        |
| depgraph plugin    | ✅          | ✅        | ✅   | Optional   |

Maven projects are notoriously difficult to resolve into final dependency
graphs. While many dependencies are declared as XML, these dependency
declarations can span many buildscripts and user settings files. What's worse:
maven plugins are often used to apply dependencies to the project, and some
maven plugins allow arbitrary executable code -- similar to gradle.

To work around this, we're using the maven cli in conjunction with the [depgraph
maven plugin](https://github.com/ferstl/depgraph-maven-plugin), version 4.0.1.
This plugin is used by some Jenkins and Apache open source projects. We also maintain support for version 3.3.0 which support JDK 7.

## Project Discovery

Find `pom.xml` files, and treat those as maven projects. Skip all subdirectories.

## Analysis

1. unpack the embedded plugin to a temporary directory
2. install it to the local maven repository `mvn org.apache.maven.plugins:maven-install-plugin:3.0.0-M1:install-file -DgroupId=com.github.ferstl -DartifactId=depgraph-maven-plugin -Dversion=4.0.1 -Dpackaging=jar -Dfile=<location>/depgraph-maven-plugin-4.0.1.jar` (uses a specific version of [Maven Install Plugin](https://maven.apache.org/plugins/maven-install-plugin/) to avoid a [bug in earlier versions](https://issues.apache.org/jira/browse/MINSTALL-110))
3. invoke the plugin in the top-level project with the command `mvn com.github.ferstl:depgraph-maven-plugin:4.0.1:aggregate -DgraphFormat=text -DmergeScopes -DreduceEdges=false -DshowVersions=true -DshowGroupIds=true -DshowOptional=true -DrepeatTransitiveDependenciesInTextGraph=true -DoutputDirectory=<tempdir>`

The `-DoutputDirectory` flag ensures output is written to a known temporary
directory regardless of any `<build><directory>` overrides in the project's POMs.
For `graphFormat`s other than `text` the data will be output to the configured
output directory. For `text`, it will additionally be output to stdout.

Submodule discovery is performed by parsing the project's POM files and building
a reachability graph (POM closure). Submodules are identified from this closure
rather than by running a separate Maven reactor invocation. We then generate a
graph that excludes all of those submodules, but with their immediate deps marked
as direct. This is because we don't want to include the users' projects in graphs,
but do want to be able to analyze the things that they depend on.

A second `:graph` (per-module) invocation is run with the command `mvn com.github.ferstl:depgraph-maven-plugin:4.0.1:graph -DgraphFormat=json -DmergeScopes -DshowDuplicates=true -DoutputFileName=fossa-depgraph-verbose.json`
to recover dependency edges that Maven resolved away as duplicates (the same
artifact reachable via multiple paths in the reactor). These duplicate-resolved
edges are merged back into the main graph. If this pass fails, a warning is
emitted and analysis continues with the aggregate result alone.

For example:

```
org:submodule1:1.0.0:compile
\- org2:pkg1:1.0.0:compile
   +- org2:pkg2:1.0.0:compile
   \- org:submodule2:1.0.0:compile
      \- org3:pkg3:1.0.0:compile
```

If `submodule1` and `submodule2` are both submodules of the project we would
report `pkg3` and `pkg1` as direct dependencies. `pkg2` is reported as a transitive
dependency. `submodule1` and `submodule2` won't be included at all. `fossa-cli`'s
graph would look like this:

```
org2:pkg1:1.0.0:compile
\- org2:pkg2:1.0.0:compile

org3:pkg3:1.0.0:compile
```
