# packages.config

The `packages.config` XML file is used in some project types to maintain the list of packages referenced by the project.

_Not sure how to read this reference?_
_Check the [Primer: strategies in FOSSA CLI](../../README.md#primer-strategies-in-fossa-cli) first!_

## Project Discovery

Walk the directory and find all files names `packages.config`.

## Analysis

| Tactic                                         | Analysis Method | Vulnerabilities | Full Graph Support | Dependency Scopes |
|------------------------------------------------|-----------------|-----------------|--------------------|-------------------|
| [Parse packages.config](#parse-packagesconfig) | Static          | :grey_question: | :grey_question:    | :grey_question:   |

### Parse `packages.config`

Parse the XML file, and collect dependency data from all `package` tags within the `packages` section:

```
<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="jQuery" version="3.1.1" targetFramework="net46" />
  <package id="NLog" version="4.3.10" targetFramework="net46" />
</packages>
```
