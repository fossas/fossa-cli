# Cocoapods Analysis

## Project Discovery

`podfile`: Find all directories containing a `Podfile` file.

`podfilelock`: Find all directories containing a `Podfile.lock` file.

## Analysis: podfile

We scan the `Podfile` text for `pod` declarations, which indicate a direct
dependency of the current project.  We also scan for a `source` declaration,
which gives us a default source to assign to all `pod` declarations without an
explicit source.

## Analysis: podfilelock

We scan the `Podfile.lock` for two particular sections: `PODS` and
`DEPENDENCIES`.  The `PODS` section describes the relationships between the
dependencies, and `DEPENDENCIES` tells us which of the dependencies the project
depends on directly.

In the following example, we have five dependencies, `one`, `two`, `three`,
`four` and `five/+zlib`. `one`, `three` and `five/+zlib` are direct dependencies, `one` depends on both
`two` and `three`, and `three` depends on `four`.

```
PODS:
  - one (1.0.0):
    - two (= 3.2.1)
    - three (= 3.2.1)
  - two (2.0.0)
  - three (3.0.0)
    - four (= 2.3.3)
  - four (4.0.0):
  - "five/+zlib (7.0.0)"

DEPENDENCIES:
  - one (> 4.4)
  - three (from `Submodules/subproject/.podspec`)
  - "five/+zlib (7.0.0)"
```