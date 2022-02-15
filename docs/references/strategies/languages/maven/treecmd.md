# Maven - Tree Command

This maven tactic uses native, `mvn dependency:tree` command to retrieve
dependency information. 

## Project Discovery

Find `pom.xml` files, and treat those as maven projects. Skip all subdirectories.

## Analysis

1. Executes `mvn dependency:tree -DoutputType=dot -DoutputFile=... --fail-at-end`
2. Parse and analyze generated dot file for dependency analysis

## References

- [Maven dependency:tree](https://maven.apache.org/plugins/maven-dependency-plugin/usage.html#dependency:tree)
