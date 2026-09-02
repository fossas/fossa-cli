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

A version specifier can name another package in the same workspace rather than a
version range: `catalog:` and `catalog:<name>`
([pnpm catalogs](https://pnpm.io/catalogs)), `workspace:`
([the workspace protocol](https://pnpm.io/workspaces#workspace-protocol)), and
`link:`. Resolving those needs the lockfile or `pnpm-workspace.yaml`, neither of
which this strategy reads, so such dependencies are skipped and a warning names
them.

This strategy is a fallback used when no lockfile is in scope — most often when
`fossa analyze` is run from inside a workspace member's own directory. Analyze
from the workspace root instead, where the lockfile resolves these specifiers to
real versions. To report on one member, see workspace build targets for
[npm](npm-lockfile.md#workspace-build-targets) or
[yarn](yarn.md#workspace-build-targets).
