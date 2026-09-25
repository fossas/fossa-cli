# package.json

package.json is a common build manifest used by both yarn and npmcli.

## Project Discovery

`npm`: Find all files named `package.json`, not descending into directories
named `node_modules`

## Analysis

`package.json` is a user modified file that specifies which dependencies are
mandatory in order to run your project. This file lists dependencies with their
version specifier. These dependencies are user specified and map nearly 1:1 with
direct dependencies, however users may also specify their desired version for
transitive dependencies which would invalidate the assumption that all
dependencies specified here are direct.

There is also a different block for development dependencies which allows us to
accurately determine part of the tags available for node.

As of _v2.19.x_, we also combine `package.json` files that are members of the
same workspace.  The files are treated as though all dependencies were found
from the same file, though we report the origins of the deps as a set of all
files.

### Workspace references

Without a lockfile, pnpm catalog references can still be resolved from
`pnpm-workspace.yaml`. For each package that uses `catalog:` or `catalog:<name>`,
FOSSA searches its directory and then its parents for the nearest workspace
file. This also works when the scan starts inside a workspace member.

The default `catalog` and named `catalogs` mappings supply the declared version
or range. For example, `"left-pad": "catalog:"` with `left-pad: ^1.3.0` in the
default catalog is reported just like `"left-pad": "^1.3.0"`. This is a partial
graph of declared dependencies; an exact installed version and transitive
dependencies still require lockfile analysis.

An unresolved catalog reference (missing or unreadable workspace file, missing
catalog, or missing package entry) is skipped with a warning naming it. FOSSA
uses only the nearest workspace file; it does not borrow missing entries from
an outer workspace. Local `workspace:` and `link:` references are also skipped
with a warning. `file:` references retain their existing behavior.

For a complete graph, analyze from the workspace root with its lockfile in
scope. See [pnpm catalogs](https://pnpm.io/catalogs) for catalog configuration.
