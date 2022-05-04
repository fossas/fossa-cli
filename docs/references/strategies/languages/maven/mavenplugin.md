# Maven plugin

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
2. install it to the local maven repository `mvn install:install-file -DgroupId=com.github.ferstl -DartifactId=depgraph-maven-plugin -Dversion=4.0.1 -Dpackaging=jar -Dfile=<location>/depgraph-maven-plugin-4.0.1.jar`
3. invoke the plugin in the top-level project with the command `mvn com.github.ferstl:depgraph-maven-plugin:4.0.1:aggregate -DgraphFormat=dot -DmergeScopes -DreduceEdges=false`