# Adding New Languages

## How language integrations work

You can add support for a languages by creating an `Analyzer` for that language.
The `Analyzer` interface describes key functionality that FOSSA CLI relies on to
perform dependency analysis:

- `Discover` does initialization for the builder and gathers environment
  context (e.g. binary paths).
- `Build` runs a default build of the module if the user requests it.
- `Analyze` returns a list of dependencies in a standard format.
- `IsBuilt` detects whether a module requires a build.

A `Dependency` is a component with a locator. Locators are built out of 3 parts:
- `Fetcher`: the component's source ecosystem (e.g. `npm` or `mvn`).
- `Package`: the package name in the ecosystem's registries.
- `Revision`: the revision (or version) name in the ecosystem's registries.
