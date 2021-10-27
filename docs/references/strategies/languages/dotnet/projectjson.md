# project.json

The `project.json` file maintains a list of packages used in a project, known as a package management format. It supersedes `packages.config` but is in turn superseded by `PackageReference` with NuGet 4.0+.

## Project Discovery

Walk the directory and find all files names `project.json`

## Analysis

Parse the JSON file and search for the `dependencies` section:

```
"dependencies": {
    "Microsoft.NETCore": "5.0.0",
    "System.Runtime.Serialization.Primitives": "4.0.10"
}
``` 
