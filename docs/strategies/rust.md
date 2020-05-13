# Rust Analysis

The rust buildtool ecosystem is nearly exclusive to `cargo`, the package manager
that ships with rust distributions.
This table may grow as other tools appear, which is expected in the next 2-3 years.

There are rare, but known cases of generic buildtools being used, like `make`,
`cmake`, and `ninja`.  These should
be ignored for a package management tool like spectrometer.

| Strategy | Direct Deps | Deep Deps | Edges | Tags         |
| ---      | ---         | ---       | ---   | ---          |
| cargo    | ✅          | ✅        | ✅    | Environment? |

## Project Discovery

Find all files with a `Cargo.toml` file, where no direct or indirect parent also
contains a `Cargo.toml` file.

For example, given the following tree, we would only examine `root/Cargo.toml`.

```
root
├── src1
│   ├── subpackage
│   │   ├── Cargo.toml
│   │   └── ...
│   └── main.rs
├── src2
│   ├── subpackage
│   │   ├── Cargo.toml
│   │   └── ...
│   └── main.rs
└── Cargo.toml
```

However, in the following tree, we examine `root/pkg1/Cargo.toml` and
`root/pkg2/Cargo.toml`, since neither has a parent directory with a `Cargo.toml`
file.

```
root
├── pkg1
│   ├── Cargo.toml
│   └── main.rs
└── pkg2
    ├── Cargo.toml
    └── main.rs
```

## Analysis

First, we invoke `cargo generate-lockfile` to trigger the lockfile build.  This
downloads the dependency graph info and almost nothing else.  Then, we read the
output of the `cargo metadata --format-version 1` command (format version 1 is JSON,
and is currently the only allowed formatting scheme).  This will arrange the data
into a slightly simpler scheme.

We can interrogate the JSON output for direct and deep dependency info, but it is
non-trivial to do so.  For more info, see the metadata schema found
[here](https://doc.rust-lang.org/cargo/commands/cargo-metadata.html)

The analysis of the JSON is slightly complex, but is roughly as follows:

* Check `.workspace_members` for a list of packages which represent the current
package and any sub packages.  For now we'll call these *local crates*.
* Examine `.resolve` (a flattened dependency graph), for each of the local crates to
fetch the list of direct dependencies of that crate.  Call this a *dependency node*.
* For each dependency node, we are able to retreive some info directly, like its
version, whether it is platform-specific, what build category it's included in
(build, dev, or standard), and what features it compiles with.  For any further
information, we have to compare this package and version with the corresponding
entry in `.packages`.

## Schema

```json
{
    /* Array of all packages in the workspace.
       It also includes all feature-enabled dependencies unless --no-deps is used.
    */
    "packages": [
        {
            /* The name of the package. */
            "name": "my-package",
            /* The version of the package. */
            "version": "0.1.0",
            /* The Package ID, a unique identifier for referring to the package. */
            "id": "my-package 0.1.0 (path+file:///path/to/my-package)",
            /* The license value from the manifest, or null. */
            "license": "MIT/Apache-2.0",
            /* The license-file value from the manifest, or null. */
            "license_file": "LICENSE",
            /* The description value from the manifest, or null. */
            "description": "Package description.",
            /* The source ID of the package. This represents where
               a package is retrieved from.
               This is null for path dependencies and workspace members.
               For other dependencies, it is a string with the format:
               - "registry+URL" for registry-based dependencies.
                 Example: "registry+https://github.com/rust-lang/crates.io-index"
               - "git+URL" for git-based dependencies.
                 Example: "git+https://github.com/rust-lang/cargo?rev=5e85ba14aaa20f8133863373404cb0af69eeef2c#5e85ba14aaa20f8133863373404cb0af69eeef2c"
            */
            "source": null,
            /* Array of dependencies declared in the package's manifest. */
            "dependencies": [
                {
                    /* The name of the dependency. */
                    "name": "bitflags",
                    /* The source ID of the dependency. May be null, see
                       description for the package source.
                    */
                    "source": "registry+https://github.com/rust-lang/crates.io-index",
                    /* The version requirement for the dependency.
                       Dependencies without a version requirement have a value of "*".
                    */
                    "req": "^1.0",
                    /* The dependency kind.
                       "dev", "build", or null for a normal dependency.
                    */
                    "kind": null,
                    /* If the dependency is renamed, this is the new name for
                       the dependency as a string.  null if it is not renamed.
                    */
                    "rename": null,
                    /* Boolean of whether or not this is an optional dependency. */
                    "optional": false,
                    /* Boolean of whether or not default features are enabled. */
                    "uses_default_features": true,
                    /* Array of features enabled. */
                    "features": [],
                    /* The target platform for the dependency.
                       null if not a target dependency.
                    */
                    "target": "cfg(windows)",
                    /* A string of the URL of the registry this dependency is from.
                       If not specified or null, the dependency is from the default
                       registry (crates.io).
                    */
                    "registry": null
                }
            ],
            /* Array of Cargo targets. */
            "targets": [
                {
                    /* Array of target kinds.
                       - lib targets list the `crate-type` values from the
                         manifest such as "lib", "rlib", "dylib",
                         "proc-macro", etc. (default ["lib"])
                       - binary is ["bin"]
                       - example is ["example"]
                       - integration test is ["test"]
                       - benchmark is ["bench"]
                       - build script is ["custom-build"]
                    */
                    "kind": [
                        "bin"
                    ],
                    /* Array of crate types.
                       - lib and example libraries list the `crate-type` values
                         from the manifest such as "lib", "rlib", "dylib",
                         "proc-macro", etc. (default ["lib"])
                       - all other target kinds are ["bin"]
                    */
                    "crate_types": [
                        "bin"
                    ],
                    /* The name of the target. */
                    "name": "my-package",
                    /* Absolute path to the root source file of the target. */
                    "src_path": "/path/to/my-package/src/main.rs",
                    /* The Rust edition of the target.
                       Defaults to the package edition.
                    */
                    "edition": "2018",
                    /* Array of required features.
                       This property is not included if no required features are set.
                    */
                    "required-features": ["feat1"],
                    /* Whether or not this target has doc tests enabled, and
                       the target is compatible with doc testing.
                    */
                    "doctest": false
                }
            ],
            /* Set of features defined for the package.
               Each feature maps to an array of features or dependencies it
               enables.
            */
            "features": {
                "default": [
                    "feat1"
                ],
                "feat1": [],
                "feat2": []
            },
            /* Absolute path to this package's manifest. */
            "manifest_path": "/path/to/my-package/Cargo.toml",
            /* Package metadata.
               This is null if no metadata is specified.
            */
            "metadata": {
                "docs": {
                    "rs": {
                        "all-features": true
                    }
                }
            },
            /* List of registries to which this package may be published.
               Publishing is unrestricted if null, and forbidden if an empty array. */
            "publish": [
                "crates-io"
            ],
            /* Array of authors from the manifest.
               Empty array if no authors specified.
            */
            "authors": [
                "Jane Doe <user@example.com>"
            ],
            /* Array of categories from the manifest. */
            "categories": [
                "command-line-utilities"
            ],
            /* Array of keywords from the manifest. */
            "keywords": [
                "cli"
            ],
            /* The readme value from the manifest or null if not specified. */
            "readme": "README.md",
            /* The repository value from the manifest or null if not specified. */
            "repository": "https://github.com/rust-lang/cargo",
            /* The default edition of the package.
               Note that individual targets may have different editions.
            */
            "edition": "2018",
            /* Optional string that is the name of a native library the package
               is linking to.
            */
            "links": null,
        }
    ],
    /* Array of members of the workspace.
       Each entry is the Package ID for the package.
    */
    "workspace_members": [
        "my-package 0.1.0 (path+file:///path/to/my-package)",
    ],
    // The resolved dependency graph for the entire workspace. The enabled
    // features are based on the enabled features for the "current" package.
    // Inactivated optional dependencies are not listed.
    //
    // This is null if --no-deps is specified.
    //
    // By default, this includes all dependencies for all target platforms.
    // The `--filter-platform` flag may be used to narrow to a specific
    // target triple.
    "resolve": {
        /* Array of nodes within the dependency graph.
           Each node is a package.
        */
        "nodes": [
            {
                /* The Package ID of this node. */
                "id": "my-package 0.1.0 (path+file:///path/to/my-package)",
                /* The dependencies of this package, an array of Package IDs. */
                "dependencies": [
                    "bitflags 1.0.4 (registry+https://github.com/rust-lang/crates.io-index)"
                ],
                /* The dependencies of this package. This is an alternative to
                   "dependencies" which contains additional information. In
                   particular, this handles renamed dependencies.
                */
                "deps": [
                    {
                        /* The name of the dependency's library target.
                           If this is a renamed dependency, this is the new
                           name.
                        */
                        "name": "bitflags",
                        /* The Package ID of the dependency. */
                        "pkg": "bitflags 1.0.4 (registry+https://github.com/rust-lang/crates.io-index)",
                        /* Array of dependency kinds. Added in Cargo 1.40. */
                        "dep_kinds": [
                            {
                                /* The dependency kind.
                                   "dev", "build", or null for a normal dependency.
                                */
                                "kind": null,
                                /* The target platform for the dependency.
                                   null if not a target dependency.
                                */
                                "target": "cfg(windows)"
                            }
                        ]
                    }
                ],
                /* Array of features enabled on this package. */
                "features": [
                    "default"
                ]
            }
        ],
        /* The root package of the workspace.
           This is null if this is a virtual workspace. Otherwise it is
           the Package ID of the root package.
        */
        "root": "my-package 0.1.0 (path+file:///path/to/my-package)"
    },
    /* The absolute path to the build directory where Cargo places its output. */
    "target_directory": "/path/to/my-package/target",
    /* The version of the schema for this metadata structure.
       This will be changed if incompatible changes are ever made.
    */
    "version": 1,
    /* The absolute path to the root of the workspace. */
    "workspace_root": "/path/to/my-package"
}
```