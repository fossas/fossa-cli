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

## Strategy: go mod graph and golist

Discovery: find go.mod files

1. We execute `go mod graph` to retrieve all edges, and all considered versions of the module.
2. We execute `go list -m -json all` to identify the main module, selected version of the module, and whether the module is direct dependency (using `Indirect` field from the command response)
3. We create a dependencies graph from using the result of (1) in the following fashion:
   a. We remove the main module from the dependency graph
   b. We label all direct modules, as a direct dependency
   c. We filter out module versions that were not selected - we know selected module versions from step (2). We do this, since the output of `go mod graph`, does return only the selected version, used in the build list! 
   d. If replacements were specified in the output of (2) we reflect them in our output as well.

## Strategy: golist

Discovery: find go.mod files

We run `go list -m -json all`, which produces, e.g.,:

```json
{
        "Path": "example.com/foo/bar",
        "Main": true,
        "Dir": "/Users/example/Codes/golang/simple",
        "GoMod": "/Users/example/Codes/golang/simple/go.mod",
        "GoVersion": "1.16"
}
{
        "Path": "github.com/kr/pretty",
        "Version": "v0.1.0",
        "Time": "2018-05-06T08:33:45Z",
        "Indirect": true,
        "Dir": "/Users/example/go/pkg/mod/github.com/kr/pretty@v0.1.0",
        "GoMod": "/Users/example/go/pkg/mod/cache/download/github.com/kr/pretty/@v/v0.1.0.mod"
}
```

- To infer direct dependencies, we filter out any module, which has `Main` field with value of true, and `Indirect` field with value of true.
- To infer transitive dependencies, we execute `go list -json all`, and parse it's output for `Imports`, `ImportPath`, `Module`, `Standard` data, and fill in the transitive dependencies.

For package dependencies that aren't using gomodules, a pseudo-version (`v0.0.0-TIMESTAMP-COMMITID`) is present instead. We use the commit ID as the version.

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

This strategy instead generates a graph of modules based on the required packages output by `go list -json -deps all`.
A Go module distributes source code for one or more packages. However, only packages are `import`ed in Go source code.
An implication of this is that the graph of module dependencies does not necessarily correspond to how different packages in a Go project depend on each other.
By looking at how packages import one another, the CLI can get more information about what packages and modules are actually used in a final build product than by looking at modules alone.

To enable a strategy that does this, run `fossa analyze` with the `--experimental-use-v3-go-resolver`.

Currently there are a few limitations to this strategy:
1. Does not exclude test dependencies.
2. Does not properly report packages with [pseudo-versions](https://go.dev/ref/mod#pseudo-versions).
3. Like our other strategies, this strategy does not yet report on path dependencies from Go `replace` directives.

## FAQ

### Why `go list -m -json all` is used instead of `go list -json -deps` to infer dependencies?

We use `go list -m -json all` in combination with the `go list -json all`, to infer direct and transitive dependencies. The reason, we do not use solely use `go list -json -deps` command at this moment, is because it does not include transitive dependencies imported with test imports. 

This go module functionality is actively being worked on, such that we can label dependencies environment (e.g. Test) correctly, for all types of Go project configurations.
