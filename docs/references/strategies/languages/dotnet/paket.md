# Paket

Paket is a dependency manager for .NET projects.
Paket enables precise and predictable control over your dependencies.

Paket manages your dependencies with three core file types:

- `paket.dependencies`, where you specify your dependencies and their versions for your entire codebase.
- `paket.references`, a file that specifies a subset of your dependencies for every project in a solution.
- `paket.lock`, a lock file that Paket generates when it runs. When you check it into source control, you get reproducible builds.

_Not sure how to read this reference?_
_Check the [Primer: strategies in FOSSA CLI](../../README.md#primer-strategies-in-fossa-cli) first!_

## Project Discovery

Walk the directory and find all Paket lock files (`paket.lock`).

## Analysis

| Tactic                                 | Analysis Method | Vulnerabilities | Full Graph Support | Dependency Scopes |
|----------------------------------------|-----------------|-----------------|--------------------|-------------------|
| [Parse `paket.lock`](#parse-paketlock) | Static          | :grey_question: | :white_check_mark: | :grey_question:   |

### Parse `paket.lock`

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

Dependencies can reference NuGet, GitHub, or a specified remote URL.
