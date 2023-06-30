# project.json

The `project.json` file maintains a list of packages used in a project,
known as a package management format.

It supersedes `packages.config`,
but is in turn superseded by `PackageReference` with NuGet 4.0+.

_Not sure how to read this reference?_
_Check the [Primer: strategies in FOSSA CLI](../../README.md#primer-strategies-in-fossa-cli) first!_

## Project Discovery

Walk the directory and find all files names `project.json`.

## Analysis

| Tactic                                     | Analysis Method | Vulnerabilities | Full Graph Support | Dependency Scopes |
|--------------------------------------------|-----------------|-----------------|--------------------|-------------------|
| [Parse `project.json`](#parse-projectjson) | Static          | :grey_question: | :grey_question:    | :grey_question:   |

### Parse `project.json`

Parse the JSON file and search for the `dependencies` section:

```
"dependencies": {
    "Microsoft.NETCore": "5.0.0",
    "System.Runtime.Serialization.Primitives": "4.0.10"
}
``` 
