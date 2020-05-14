# Dep

Dep is an alternative toolchain for golang dependency management. Though go
modules are the blessed form of dependency management, dep is exploring
alternatives in the dependency management space, and hasn't been deprecated.

## Project Discovery

`gopkglock`: Find all files named `Gopkg.lock`

`gopkgtoml`: Find all files named `Gopkg.toml`

## Analysis: gopkglock

We parse [projects][depprojects] from Gopkg.lock. This is more comprehensive
than GopkgToml, as it contains pinned versions of all of our direct _and_
transitive dependencies. We also pick up on `source` locations from GopkgLock

[depprojects]: https://golang.github.io/dep/docs/Gopkg.toml.html#dependency-rules-constraint-and-override

## Analysis: gopkgtoml

We parse [dependency rules][deprules] from Gopkg.toml. `override`s are similar
to [gomod replaces](gomodules.md).

[deprules]: https://golang.github.io/dep/docs/Gopkg.toml.html#dependency-rules-constraint-and-override
