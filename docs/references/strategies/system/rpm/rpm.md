# RPM analysis

RPM packages managed by system package managers like `dnf` and `yum` are built
using SPEC files.  We are able to analyze these files to determine runtime
dependencies.

| Strategy | Direct Deps        | Transitive Deps | Edges | Tags        | Container Scanning |
| -------- | ------------------ | --------- | ----- | ----------- | ------------------ |
| rpm-spec | :white_check_mark: | :x:       | :x:   | Environment | :white_check_mark: |

## Project Discovery

RPM SPEC files have strict naming conventions.  They must be in the form of
`<package-name>.spec`, e.g.: `binutils.spec`.  We scan any file with a `.spec`
file extension.

## Analysis

Currently, we scan for lines which start with `Requires:`, followed by at
least one space.  On this line we extract EXACTLY ONE package, which may
optionally specify a single version constraint.  This package and its
constraint are considered a single, direct, runtime dependency.

## Limitations

The SPEC file format is simple, yet robust.  We currently do not support all of
its features yet.  Notably, we do not support the following:

* Combinatoric version constraints, such as `(>= 1.2 and != 1.4) or = 0.9`
  * Parentheses, as well as the `and` and `or` constructs are not supported yet.
* Build-time requirements
  * We currently parse these, since the form is identical to runtime dependencies
  (`BuildRequires:` vs. `Requires:`), but we do not know if this is valuable
  for users, since some of these dependencies are tools like compilers (`gcc`
  is a common build-time dependency).
* Macros, such as `xxxxxx-%{?_isa}`, or `xxxxx = %{version}-%{release}`
  * SPEC files allow for a few macros to dynamically determine package names/versions.
    We do not yet support this, although we may support it in the future.
