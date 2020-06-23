# RPM analysis

RPM packages managed by system package managers like `dnf` and `yum` are built
using SPEC files.  We are able to analyze these files to determine build-time
dependencies.

| Strategy | Direct Deps | Deep Deps | Edges | Tags         |
| ---      | ---         | ---       | ---   | ---          |
| rpm-spec | ✅          | ❌        | ❌    | Environment  |

## Project Discovery

RPM SPEC files have strict naming conventions.  They must be in the form of
`<package-name>.spec`, e.g.: `binutils.spec`.  We scan any file with a `.spec`
file extension.

## Analysis

Currently, we scan for lines which start with `BuildRequires:`, followed by at
least one space.  On this line we extract EXACTLY ONE package, which may
optionally specify a single version constraint.  This package and its
constraint are considered a single, direct, build-time dependency.

## Limitations

The SPEC file format is simple, yet robust.  We currently do not support all of
its features yet.  Notably, we do not support the following:

* Combinatoric version constraints, such as `(>= 1.2 and != 1.4) or = 0.9`
  * Parentheses, as well as the `and` and `or` constructs are not supported yet.
* Runtime requirements
  * This is easy to implement, but may not be relevant or necessary for real-life
    analysis.
* Macros, such as `xxxxxx-%{?_isa}`, or `xxxxx = %{version}-%{release}`
  * SPEC files allow for a few macros to dynamically determine package names/versions.
    We do not yet support this, although we may support it in the future.
