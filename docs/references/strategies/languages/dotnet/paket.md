# Paket

| Strategy          | Direct Deps | Transitive Deps | Edges |
|-------------------|-------------|-----------------|-------|
| [paket](paket.md) | ✅           | ✅               | ✅     |

Paket is a dependency manager for .NET projects. Paket enables precise and predictable control over your dependencies

Paket manages your dependencies with three core file types:

`paket.dependencies`, where you specify your dependencies and their versions for your entire codebase.
`paket.references`, a file that specifies a subset of your dependencies for every project in a solution.
`paket.lock`, a lock file that Paket generates when it runs. When you check it into source control, you get reproducible builds.

## Project Discovery

Walk the directory and find all lock files (`paket.lock`)

## Analysis
Parse the lock file, and construct a dependency graph:

```
NUGET
  remote: nuget.com
    one (1.0.0)
      two (>1.0.0)
    two (2.0.0)

HTTP
  remote: custom-site.com
    three (3.0.0)
```

dependencies can come from either NuGet, Github, or a specified remote URL.