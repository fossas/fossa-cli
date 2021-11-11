# Golang Modules

Golang 1.11 has first-class support for "modules", which is now the preferred
way to do dependency management.

## Project Discovery

Find all files named `go.mod`

## Analysis: golist

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

## Analysis: gomod

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


## FAQ

### Why `go list -m -json all` is used instead of `go list -json -deps` to infer dependencies?

We use `go list -m -json all` in combination with the `go list -json all`, to infer direct and transitive dependencies. The reason, we do not use solely use `go list -json -deps` command at this moment, is because it does not include transitive dependencies imported with test imports. 

This go module functionality is actively being worked on, such that we can label dependencies environment (e.g. Test) correctly, for all types of golang project configurations.