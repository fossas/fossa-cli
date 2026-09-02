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
