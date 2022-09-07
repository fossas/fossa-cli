# Fortran Analysis

Currently, we only support analysis of fortran project which are using [fortran package manager](https://github.com/fortran-lang/fpm).

| Strategy | Direct Deps        | Deep Deps | Edges | Classifies Dev & Test Deps | Container Scanning (experimental) |
| -------- | ------------------ | --------- | ----- | -------------------------- | --------------------------------- |
| fpm      | :white_check_mark: | :x:       | :x:   | :white_check_mark:         | :white_check_mark:                |

## Project Discovery

Find a file named `fpm.toml`. If `fpm.toml` is found, we do not scan in the `build` directory for more Fortran projects.

## Analysis

1. Parse `fpm.toml` file to identify direct dependencies.

## Limitations

- We do not report [path dependencies](https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md#local-dependencies).
- We do not report test or [development dependencies](https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md#development-dependencies).
- We only report direct dependencies.

## Example

Create a Fortran project managed by `fpm`. Execute `fpm new project_name`. This will create a directory named project_name with a manifest file, and a few example source files.

Modify `fpm.toml` file to include direct dependency:

```toml
name = "fortran_example"
version = "0.1.0"
license = "license"
author = "Jane Doe"
maintainer = "jane.doe@example.com"
copyright = "Copyright 2021, Jane Doe"

[build]
auto-executables = true
auto-tests = true
auto-examples = true

[install]
library = false

[dependencies]
toml-f = { git = "https://github.com/toml-f/toml-f", tag = "v0.2.1" }
```

Now modify `app/main.f90` to use `tomlf`:

```fortran
program main
  use tomlf, only: get_tomlf_version
  implicit none
  
  integer :: major
  integer :: minor
  integer :: patch
  call get_tomlf_version(major, minor, patch)
  
  print *, "tomlf's major version is: ", major
  print *, "tomlf's minor version is: ", minor
  print *, "tomlf's patch version is: ", patch
end program main
```

Execute `fpm build` to build the fortran project.

Now you can call `fpm run` to execute the built application, here is an example output from this application:
```
 tomlf's major version is:            0
 tomlf's minor version is:            2
 tomlf's patch version is:            1
```

To analyze this project using fossa CLI, execute `fossa analyze --output --debug`. This will print identified dependency graphs and debug logs only. It will not upload the performed analysis to any endpoint.

## References

- [Fortran Package Manager](https://github.com/fortran-lang/fpm)