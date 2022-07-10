# project.assets.json

The `project.assets.json` file is used in `.NET Core` projects to manage dependencies and other resources

## Project Discovery

Walk the directory and find all files names `project.assets.json`

## Analysis

> Note, `fossa-cli` expects that the project is compiled, and built prior to using `fossa analyze` command.
 
Parse the JSON file, and construct a full dependency graph (direct and transitive dependencies).
 
From the `project.assets.json`, `fossa-cli` uses:
 
- `targets`: To infer edges between dependencies of a framework.
- `project`: To infer which dependencies are direct dependencies for the target framework.
 
For example,
 
```json
{
     "version": 1,
     "targets": {
           ".NETFramework,Version=v4.0": {
                 "one/1.0.0": { // Dependency Name and Version
                       "type": "package",
                       "dependencies": {
                             "three": "3.0.0"
                       }
                 },
                 "three/3.0.0": {
                       "type": "package"
                 },
           }
     },
     "project": {
           "frameworks": {
                 "net4.0": {
                       "targetAlias": "net4.0",
                       "dependencies": {
                             "one": {}, // Direct Dependency
                       }
                 }
           }
     }
}
```
 
* Key heading under `targets.$frameworkName` are used to infer resolved dependency. For instance `one/1.0.0` will be inferred as a dependency with name of `one` and version of `1.0.0`.
 
* For each dependency, all entries under `dependencies` will form edges. From the previous example, `fossa-cli` will infer edge from dependency `one` to dependency `three`.
 
* Dependencies listed under `project.$frameworkName` are used to infer if the dependency is direct or not. From aforementioned example, `one` will be considered a direct dependency.
 
* Any dependency of type `"project"` will be ignored from the analysis.