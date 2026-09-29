# C and C++ Analysis

When developing C and C++ projects, no standard package manager is used.
Consequently, we use statistical analysis of the project to determine likely vendored dependencies,
and support inspecting output binaries for dynamically linked dependencies on some platforms.

We do support the Conan package manager as part of our regular analysis strategies.

| Strategy          | Direct Deps        | Transitive Deps    | Edges                | Container Scanning |
|-------------------|--------------------|--------------------|----------------------|--------------------|
| `conan`           | :white_check_mark: | :white_check_mark: | :white_check_mark:   | :white_check_mark: |
| `detect-vendored` | :white_check_mark: | :x:                | :x:                  | :x:                |
| `detect-dynamic`  | :white_check_mark: | :white_check_mark: | :x:                  | :x:                |


## Project Discovery

- `conan`: The root of the scan directory and any sub-directories which contain conan manifest files are considered "projects".
- `detect-vendored`: The root of the scan directory is assumed to be the only "project".
- `detect-dynamic`: The binary indicated by the argument is assumed to be the only "project".

## Analysis

- `conan`: Part of the strategies that a normal analysis runs.
- `detect-vendored`: Run `fossa analyze --detect-vendored`.
  - To _only_ detect vendored dependencies, run `fossa analyze --detect-vendored --only-target vsi`.
- `detect-dynamic`: Run `fossa analyze --detect-dynamic <BINARY_PATH>`.
  - This inspects the binary at `BINARY_PATH` for dynamically linked dependencies.
  - This requires specific platform support, see extended documentation under _further reading_.

Further reading:
- [`conan`](conan.md)
- [`detect-vendored`](../../../subcommands/analyze/detect-vendored.md) in [`subcommands/analyze`](../../../subcommands/analyze.md)
- [`detect-dynamic`](../../../subcommands/analyze/detect-dynamic.md) in [`subcommands/analyze`](../../../subcommands/analyze.md)
