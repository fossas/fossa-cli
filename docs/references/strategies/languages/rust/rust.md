# Rust Analysis

The rust buildtool ecosystem is nearly exclusive to `cargo`, the package manager
that ships with rust distributions. There are rare, but known cases of generic
buildtools being used, like `make`, `cmake`, and `ninja`.  These cases are not
handled here.

| Strategy | Direct Deps        | Transitive Deps          | Edges              | Tags        | Container Scanning |
| -------- | ------------------ | ------------------ | ------------------ | ----------- | ------------------ |
| cargo    | :white_check_mark: | :white_check_mark: | :white_check_mark: | Environment | :x:                |

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

**NOTE**: If we do not attempt to regenerate the lockfile, we may try to download
every dependency in the tree.  This can take upwards of 10 minutes even for small
projects, which is wasteful and unnecessary.  However, if the lockfile has recently
been generated, it will try to update the package index.  This usually takes about
2-10 seconds, which is potentially wasteful, but far less so than the 10 minutes
that it would take to download entire crates.

We can interrogate the JSON output for direct and transitive dependency info, but it is
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
