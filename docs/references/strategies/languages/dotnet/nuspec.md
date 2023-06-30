# nuspec

A `.nuspec` file is an XML manifest that contains package metadata for Nuget packages.
This manifest is used both to build the package and to provide information to consumers.

_Not sure how to read this reference?_
_Check the [Primer: strategies in FOSSA CLI](../../README.md#primer-strategies-in-fossa-cli) first!_

## Discovery

Walk the scan directory and find all files with a  `.nuspec` suffix.

## Analysis

| Tactic                                          | Analysis Method | Vulnerabilities | Full Graph Support | Dependency Scopes |
|-------------------------------------------------|-----------------|-----------------|--------------------|-------------------|
| [Dependencies Metadata](#dependencies-metadata) | Static          | :grey_question: | :grey_question:    | :grey_question:   |
| [Dependency Groups](#dependency-groups)         | Static          | :grey_question: | :grey_question:    | :grey_question:   |

### Dependencies Metadata

The <dependencies> element within <metadata> contains
any number of <dependency> elements that identify other packages
upon which the top-level package depends.

Example:

```
<?xml version="1.0"?>
<package >
  <metadata>
    <id>GenericServices</id>
    <version>1.0.10</version>
    <title>GenericServices</title>
    <authors>Jon Smith</authors>
    <owners>Jon Smith</owners>
    <dependencies>
      <dependency id="GenericLibsBase" version="1.0.1" />
      <dependency id="EntityFramework" version="6.1.3" />
      <dependency id="AutoMapper" version="4.2.1" />
      <dependency id="DelegateDecompiler.EntityFramework" version="0.18" />
    </dependencies>
  </metadata>
</package>
```

### Dependency Groups

As an alternative to a single flat list, dependencies can be specified
according to the framework profile of the target project
using <group> elements within <dependencies>.

```
<?xml version="1.0"?>
<package >
  <metadata>
    <id>GenericServices</id>
    <version>1.0.10</version>
    <title>GenericServices</title>
    <authors>Jon Smith</authors>
    <owners>Jon Smith</owners>
    <dependencies>
      <group>
        <dependency id="RouteMagic" version="1.1.0" />
      </group>
      <group targetFramework=".NETFramework4.7.2">
        <dependency id="jQuery" version="1.6.2" />
        <dependency id="WebActivator" version="1.4.4" />
      </group>
    </dependencies>
  </metadata>
</package>
```
