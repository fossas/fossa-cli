# Golang Modules

Golang 1.11 has first-class support for "modules", which is now the preferred
way to do dependency management.

## Project Discovery

Find all files named `go.mod`

## Analysis: golist

Discovery: find go.mod files

We run `go list -m all`, which produces, e.g.,:

```
github.com/skyrocknroll/go-mod-example
github.com/alecthomas/template v0.0.0-20160405071501-a0175ee3bccc
github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf
github.com/davecgh/go-spew v1.1.1
github.com/gorilla/mux v1.6.2
github.com/konsorten/go-windows-terminal-sequences v1.0.1
github.com/pmezard/go-difflib v1.0.0
github.com/sirupsen/logrus v1.2.0
github.com/stretchr/objx v0.1.1
github.com/stretchr/testify v1.2.2
golang.org/x/crypto v0.0.0-20180904163835-0709b304e793
golang.org/x/sys v0.0.0-20180905080454-ebe1bf3edb33
gopkg.in/alecthomas/kingpin.v2 v2.2.6
```

where the first line references the current module, and the remaining lines are
package imports and pinned version separated by a space. For package
dependencies that aren't using gomodules, a pseudo-version
(`v0.0.0-TIMESTAMP-COMMITID`) is present instead. We use the commit ID as the
version.

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
