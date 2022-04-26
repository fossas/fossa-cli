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

We also look at the `EXTERNAL SOURCES` section to try and resolve locally vendored Cocoapods. If we see a locally vendored Cocoapod using either `:podspec` or `:path` to a local directory, we'll read the Podspec at that directory and also upload that dependency if it has a supported `source`. We currently only support the `git` source.

## Limitations

- Pods sourced from http path are not supported (e.g `pod 'JSONKit', :podspec => 'https://example.com/JSONKit.podspec'`).
- Pods sourced from subversion, mercurial, and bazaar are not supported.
- Plugins in Podfiles are ignored.
