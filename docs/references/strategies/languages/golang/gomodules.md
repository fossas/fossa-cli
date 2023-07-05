# Golang Modules

Golang 1.11 has first-class support for "modules", which is now the preferred
way to do dependency management.

## Project Discovery

Find all files named `go.mod`

## Analysis

We attempt to perform the following strategies in order (first succeeding strategy's results are selected):

1. go mod graph and golist
2. go list
3. gomod

## Strategy: golist

Discovery: find go.mod files

We run `go list -json -deps all`, which produces, e.g.,:

```json
{
        "Dir": "go/pkg/mod/github.com/cespare/xxhash/v2@v2.1.2",
        "ImportPath": "github.com/cespare/xxhash/v2",
        "Name": "xxhash",
        "Doc": "Package xxhash implements the 64-bit variant of xxHash (XXH64) as described at http://cyan4973.github.io/xxHash/.",
        "Root": "go/pkg/mod/github.com/cespare/xxhash/v2@v2.1.2",
        "Module": {
                "Path": "github.com/cespare/xxhash/v2",
                "Version": "v2.1.2",
                "Time": "2021-08-24T09:58:46Z",
                "Indirect": true,
                "Dir": "go/pkg/mod/github.com/cespare/xxhash/v2@v2.1.2",
                "GoMod": "go/pkg/mod/cache/download/github.com/cespare/xxhash/v2/@v/v2.1.2.mod",
                "GoVersion": "1.11"
        },
...
}
```

We generate a graph of *packages* and then resolve these back to their parent modules.
Every `Main` module we find in this graph will have its dependencies promoted to be a direct dependency.

The reason that we started with a graph of packages is because a Go module distributes source code for one or more packages.
However, only packages are `import`ed in Go source code.
An implication of this is that the graph of module dependencies does not necessarily correspond to how different packages in a Go project depend on each other.
By looking at how packages import one another, the CLI can get more information about what packages and modules are actually used in a final build product than by looking at modules alone.
This should eliminate some false positives found by older tactics that use `go list -m`.

Currently, this strategy does not yet include path dependencies or their transitive deps from Go `replace` directives.

This strategy was previously availabe only under the `--experimental-use-v3-go-resolver` flag but is now the default.
For more information about this transiton please see this [document](./v3-go-resolver-transition-qa.md).

## Strategy: gomod

We parse the go.mod file, which looks something like:

```
module our/package/path

require (
    github.com/example/one v1.2.3
    github.com/example/two v2.3.4
)

replace github.com/example/two => github.com/example/other v2.0.0
```

where:

- `replace` rewrites `require`s. In this example, our requires resolve to
  `[github.com/example/one v1.2.3, github.com/example/other v2.0.0]`

## Experimental Strategy: Use Go List on Packages

## FAQ

### Why `go list -m -json all` is used instead of `go list -json -deps` to infer dependencies?

We use `go list -m -json all` in combination with the `go list -json all`, to infer direct and transitive dependencies. The reason, we do not use solely use `go list -json -deps` command at this moment, is because it does not include transitive dependencies imported with test imports.

This go module functionality is actively being worked on, such that we can label dependencies environment (e.g. Test) correctly, for all types of Go project configurations.
