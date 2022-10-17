# Go Analysis

Historically, the Go buildtool ecosystem has been very fragmented: many
tools like dep, Glide, Govendor, etc have tried to address issues around build
reproducibility, versioning and vendoring of dependencies.

As of Go 1.11, we have first-class support for "modules", which solves
issues previously seen around versioning and build reproducibility.

Because modules address those issues so well, the other tooling has largely gone
into maintenance mode, with the notable exception of dep. As such, Go
analysis in fossa-cli primarily targets Go 1.11+ modules and dep. Support
for Glide is also included, because it's still commonly used.

| Strategy               | Direct Deps        | Transitive Deps    | Edges     | Container Scanning |
| ---------------------- | ------------------ | ------------------ | --------- | ------------------ |
| [golist](gomodules.md) | :white_check_mark: | :white_check_mark: | :warning: | :x:                |
| [gomod](gomodules.md)  | :white_check_mark: | :x:                | :warning: | :x:                |
| [gopkglock](godep.md)  | :white_check_mark: | :white_check_mark: | :warning: | :x:                |
| [gopkgtoml](godep.md)  | :white_check_mark: | :warning:          | :warning: | :x:                |
| [glide](glide.md)      | :white_check_mark: | :white_check_mark: | :X:       | :white_check_mark: |

## 🔶 Edges and transitive dependencies

Most strategies (except for gomod, where it would be redundant -- golist
supersedes gomod) use `go list -json all` to hydrate edges and transitive
dependencies. Package imports are recursively traversed, ignoring `Standard`
(system) packages.

`go list` behaves slightly differently depending on the context:

- in a gomodules project, packages can contain a `Module` field that contains a
pinned `Version`. The version will otherwise be unspecified
- `go list` includes vendored packages in both gomodules and non-gomodules projects.
