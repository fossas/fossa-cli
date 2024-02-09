# Go Modules

Go 1.11 has first-class support for "modules", which is now the preferred
way to do dependency management.

## Project Discovery

Find all files named `go.mod`

## Analysis

FOSSA CLI attempts to perform the following strategies in order (the results of the first succeeding strategy are selected):

1. go list
2. gomod

## Strategy: golist

Discovery: find go.mod files

FOSSA CLI runs `go list -json -e -deps all` which produces something like:

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

FOSSA CLI generates a graph of *packages* and then resolves these back to their parent modules.
Every `Main` module it finds in this graph will have its dependencies promoted to be a direct dependency of the project.

The reason that FOSSA CLI starts with a graph of packages is because Go modules distribute source code for one or more packages.
However, only _packages_, not _modules_, are `import`ed in Go source code.
An implication of this is that the graph of _module_ dependencies does not necessarily correspond to the graph of _package_ dependencies,
which are the real deciding factor in whether a given module is actually used in the end program.
By looking at how packages import one another, FOSSA CLI can get more information about what packages
(and therefore modules) are actually used in a final build product than by looking at modules alone.
This should eliminate some false positives found by tactics in older versions of FOSSA CLI that use `go list -m`.

Currently, this strategy does not yet include path dependencies or their transitive deps from Go `replace` directives.

This strategy was previously available only under the `--experimental-use-v3-go-resolver` flag but is now the default.
For more information about this transition please see this [document](./v3-go-resolver-transition-qa.md).

### Experimental: Path dependencies

`golist` strategy, supports experimental [path dependencies](./../../../experimental/path-dependency.md). It is not, 
enabled by default, and has to be explicitly enabled by using `--experimental-analyze-path-dependencies` flag with `fossa analyze` command.

In your project, you may have path dependencies, which are sourced from file system. For example, 
consider `go.mod` file, which looks something like:

```go
module tester

go 1.14

require github.com/Masterminds/squirrel v1.4.0
replace github.com/Masterminds/squirrel => ../vendor/squirrel
```

With this `go.mod` file and with [experimental path dependencies functionality](./../../../experimental/path-dependency.md) enabled, `fossa-cli` will
correctly, include `../vendor/squirrel` in the dependency findings. It will identify transitive dependencies
originating from package at `../vendor/squirrel`. It will also perform license scan in the directory to identify
any license and copyright obligations. 

Without [experimental path dependencies functionality](./../../../experimental/path-dependency.md) enabled, `fossa-cli` will not include `../vendor/squirrel` 
in the dependency graph. Further, it will not show [path](https://docs.fossa.com/docs/dependencies-browser#transitive-dependencies) in FOSSA UI 
for any of it's transitive dependencies.

To learn more, refer to [path dependency documentation](./../../../experimental/path-dependency.md)

## Strategy: gomod

FOSSA CLI parses the go.mod file, which looks something like:

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

### Why do I see a dependency in `go.mod`, but it is not reflected in FOSSA?

To explain how this can be the case, it's important to note that just because a package is in `go.mod` doesn't mean that it's actually used in the project;
and just because it's in `go.mod` without an `// indirect` comment doesn't mean it's actually direct.
Instead, [the Go language defines direct dependencies as](https://go.dev/ref/mod#glos-direct-dependency):

> A package whose path appears in an `import` declaration in a `.go` source file for a package or test in the main module, 
> or the module containing such a package.

This disconnect, where the `go.mod` file is not representative of the actual project,
occurs because Go considers packages in the `go.mod` advisory
(as in, it is not a build error to build a project that does not use a referenced package).
It also considers the `// indirect` comment to be "cosmetic", which means that the only way
this comment gets added or removed is via `go mod tidy` (or manually, by editing the `go.mod` directly).

To illustrate this, we have created a very simple reproduction case and copied the results below.
Note that at the end, despite the `go.mod` file being outdated (the dependency is neither used, nor marked `// indirect`),
Go did not complain at all when the project was built. 

Shell commands are prefixed by `;`, and comments about those commands are prefixed by `#`:
```
; fossa -V
fossa-cli version 3.8.6 (revision b2657cb78351 compiled with ghc-9.0)

# Created the initial repro case project.
; cat go.mod
module github.com/jssblck/gomodtest

go 1.20

require github.com/cenkalti/backoff/v4 v4.2.1

# Used the `backoff` package in the actual code.
; cat main.go
package main

import (
	"fmt"

	"github.com/cenkalti/backoff/v4"
)

func main() {
	fmt.Printf("default initial backoff interval: %v\n", backoff.DefaultInitialInterval)
}

# FOSSA reports `backoff` as a dependency.
; fossa analyze -o 2> /dev/null | jq '.sourceUnits'
[
  {
    "AdditionalDependencyData": null,
    "Build": {
      "Artifact": "default",
      "Dependencies": [
        {
          "imports": [],
          "locator": "go+github.com/cenkalti/backoff/v4$v4.2.1"
        }
      ],
      "Imports": [
        "go+github.com/cenkalti/backoff/v4$v4.2.1"
      ],
      "Succeeded": true
    },
    "Data": null,
    "Files": null,
    "GraphBreadth": "complete",
    "Info": null,
    "Manifest": "/Users/jessica/projects/scratch/gomodtest/",
    "Name": "/Users/jessica/projects/scratch/gomodtest/",
    "OriginPaths": [
      "go.mod"
    ],
    "Type": "gomod"
  }
]

# Edited to remove references to the `backoff` package.
; cat main.go
package main

import (
	"fmt"
)

func main() {
	fmt.Println("not using backoff!")
}

# Left `go.mod` unchanged.
; cat go.mod
module github.com/jssblck/gomodtest

go 1.20

require github.com/cenkalti/backoff/v4 v4.2.1

# FOSSA no longer reports `backoff` as a dependency.
; fossa analyze -o 2> /dev/null | jq '.sourceUnits'
[
  {
    "AdditionalDependencyData": null,
    "Build": {
      "Artifact": "default",
      "Dependencies": [],
      "Imports": [],
      "Succeeded": true
    },
    "Data": null,
    "Files": null,
    "GraphBreadth": "complete",
    "Info": null,
    "Manifest": "/Users/jessica/projects/scratch/gomodtest/",
    "Name": "/Users/jessica/projects/scratch/gomodtest/",
    "OriginPaths": [
      "go.mod"
    ],
    "Type": "gomod"
  }
]

# Even though `go.mod` is outdated, it can still build with no warnings or errors.
; go build
; ./gomodtest
not using backoff!
```

As a concrete step towards resolving this sort of discrepancy, we recommend running `go mod tidy` on projects regularly;
this command should synchronize the `go.mod` file with the actual state of the project.

#### Test Dependencies

Sometimes the above procedure may uncover a dependency that is not reported by FOSSA but that is also not removed by `go mod tidy`.
The other reason that a dependency may appear in `go.mod` but not in FOSSA is that it is a test dependency.
`go.mod` itself does not label dependencies as being used only in tests, but FOSSA's Go module strategy identifies and excludes test-only dependencies.
To verify that a direct dependency is not a test-only dependency, we recommend searching the project source code for where a package from the module is imported.
Generally, for a declaration in `go.mod` like:

```
	github.com/prometheus/client_golang v1.12.2
```

You can search for its imports using a command like:

```sh
$ find <project_directory> -name \*.go -exec grep -Hn "github.com/prometheus/client_golang" {} \;
```
If `find`/`grep` are not available on your system, you can install a tool like [ripgrep](https://github.com/BurntSushi/ripgrep?tab=readme-ov-file#ripgrep-rg) to perform the same operation:
```sh
rg 'github.com/prometheus/client_golang' -F -l
```

If the import only appears in source files that end in `_test.go`, it is a test-only dependency.
You can read more about how tests are defined in Go [here](https://go.dev/doc/tutorial/add-a-test).
